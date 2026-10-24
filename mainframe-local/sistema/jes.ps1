# ======================================================================
#  MAINFRAME LOCAL DO CURSO - SIMULADOR DE JES/JCL
#  Executa um subconjunto do JCL do z/OS no computador do aluno:
#  JOB, EXEC (PGM= e PROC IGYWCL), DD, SET, IF/THEN/ELSE/ENDIF, COND,
#  IEFBR14, IEBGENER, SORT (via GCSORT), IDCAMS (DEFINE GDG, DELETE,
#  REPRO, LISTCAT, SET MAXCC), programas COBOL (via GnuCOBOL) e GDG.
#  Datasets ficam em catalogo\, a saida dos jobs em spool\.
#  Mensagens com prefixo SIM sao do simulador (nao existem no z/OS).
# ======================================================================
param(
  [Parameter(Position=0)][string]$Alvo,
  [switch]$Ver,
  [switch]$Listar
)
$ErrorActionPreference = 'Stop'
$Sis  = $PSScriptRoot
$Raiz = Split-Path -Parent $Sis
$SEP  = [IO.Path]::DirectorySeparatorChar
$LAT  = [Text.Encoding]::GetEncoding(28591)
function J { $p = $args[0]; for ($i = 1; $i -lt $args.Count; $i++) { $p = [IO.Path]::Combine($p, $args[$i]) }; return $p }
$Cat   = J $Raiz 'catalogo'
$Atr   = J $Cat '.atrib'
$Spool = J $Raiz 'spool'
$Work  = J $Sis 'work'
foreach ($d in @($Cat, $Atr, $Spool, $Work)) { if (-not (Test-Path $d)) { [void](New-Item -ItemType Directory -Path $d) } }

# ---------------------------------------------------------------- config
$CFG = @{ USUARIO = 'ALUNO'; COBC = 'cobc'; GCSORT = 'gcsort'; RUNNER = ''; EXE = '.exe'; MOD = '.dll' }
$cfgFile = J $Sis 'config.txt'
if (Test-Path $cfgFile) {
  foreach ($l in [IO.File]::ReadAllLines($cfgFile)) {
    if ($l -match '^\s*([A-Z]+)\s*=\s*(.*?)\s*$') { $CFG[$matches[1]] = $matches[2] }
  }
}
$USER = $CFG.USUARIO.ToUpper()
if ($USER.Length -gt 7) { $USER = $USER.Substring(0, 7) }

function Rel([string]$abs) {
  if ($abs.StartsWith($Raiz)) { return $abs.Substring($Raiz.Length).TrimStart($SEP) }
  return $abs
}
function Hora { return (Get-Date).ToString('HH.mm.ss') }

# ---------------------------------------------------------------- catalogo
function DsFile([string]$dsn) { return (J $Cat $dsn) }
function DsExists([string]$dsn) { return (Test-Path -LiteralPath (DsFile $dsn)) }
function DsIsPds([string]$dsn) { return (Test-Path -LiteralPath (DsFile $dsn) -PathType Container) }
function GetAttr([string]$dsn) {
  $f = J $Atr $dsn
  $a = @{ RECFM = 'FB'; LRECL = 80 }
  if (Test-Path -LiteralPath $f) {
    foreach ($kv in ([IO.File]::ReadAllText($f).Trim() -split '\s+')) {
      if ($kv -match '^(\w+)=(.*)$') { $a[$matches[1]] = $matches[2] }
    }
    $a.LRECL = [int]$a.LRECL
  }
  return $a
}
function SetAttr([string]$dsn, $recfm, $lrecl) {
  [IO.File]::WriteAllText((J $Atr $dsn), "RECFM=$recfm LRECL=$lrecl")
}
function DelAttr([string]$dsn) { $f = J $Atr $dsn; if (Test-Path -LiteralPath $f) { Remove-Item -LiteralPath $f -Force } }
function GdgBaseFile([string]$base) { return (J $Atr ($base + '.GDGBASE')) }
function GdgIsBase([string]$base) { return (Test-Path -LiteralPath (GdgBaseFile $base)) }
function GdgInfo([string]$base) {
  $t = [IO.File]::ReadAllText((GdgBaseFile $base))
  $i = @{ LIMIT = 255; SCRATCH = $true }
  if ($t -match 'LIMIT=(\d+)') { $i.LIMIT = [int]$matches[1] }
  if ($t -match 'NOSCRATCH') { $i.SCRATCH = $false }
  return $i
}
function GdgGens([string]$base) {
  $r = @()
  foreach ($f in (Get-ChildItem -LiteralPath $Cat -File)) {
    if ($f.Name.StartsWith($base + '.G') -and $f.Name.Substring($base.Length) -match '^\.G(\d{4})V00$') { $r += [int]$matches[1] }
  }
  return ($r | Sort-Object)
}
function GdgName([string]$base, [int]$n) { return ('{0}.G{1:D4}V00' -f $base, $n) }
function MemberFile([string]$dsn, [string]$mem) {
  $dir = DsFile $dsn
  foreach ($f in (Get-ChildItem -LiteralPath $dir -File)) {
    if ($f.Name.ToUpper() -eq $mem -or $f.BaseName.ToUpper() -eq $mem) { return $f.FullName }
  }
  return $null
}

# ---------------------------------------------------------------- registros
function ReadRecords($fileAbs, [int]$lrecl, [bool]$isText) {
  $recs = New-Object System.Collections.Generic.List[byte[]]
  if ($isText) {
    foreach ($l in [IO.File]::ReadAllLines($fileAbs, $LAT)) { $recs.Add($LAT.GetBytes($l.PadRight($lrecl).Substring(0, $lrecl))) }
    return ,$recs
  }
  $b = [IO.File]::ReadAllBytes($fileAbs)
  if ($b.Length -gt 0 -and ($b -contains 10)) {
    # arquivo com quebra de linha: trata como texto
    foreach ($l in ($LAT.GetString($b) -split "`r?`n")) { if ($l.Length -gt 0) { $recs.Add($LAT.GetBytes($l.PadRight($lrecl).Substring(0, $lrecl))) } }
    return ,$recs
  }
  for ($i = 0; $i + $lrecl -le $b.Length; $i += $lrecl) { $r = New-Object byte[] $lrecl; [Array]::Copy($b, $i, $r, 0, $lrecl); $recs.Add($r) }
  return ,$recs
}
function WriteRecords($fileAbs, $recs, [int]$lrecl, [bool]$isText, [bool]$append) {
  if ($isText) {
    $sb = New-Object Text.StringBuilder
    foreach ($r in $recs) { [void]$sb.Append($LAT.GetString($r).TrimEnd()); [void]$sb.Append("`r`n") }
    if ($append) { [IO.File]::AppendAllText($fileAbs, $sb.ToString(), $LAT) } else { [IO.File]::WriteAllText($fileAbs, $sb.ToString(), $LAT) }
    return
  }
  $ms = New-Object IO.MemoryStream
  foreach ($r in $recs) {
    $x = New-Object byte[] $lrecl
    for ($k = 0; $k -lt $lrecl; $k++) { $x[$k] = 32 }
    [Array]::Copy($r, 0, $x, 0, [Math]::Min($r.Length, $lrecl))
    $ms.Write($x, 0, $lrecl)
  }
  if ($append -and (Test-Path -LiteralPath $fileAbs)) {
    $fs = [IO.File]::Open($fileAbs, 'Append'); $ms.WriteTo($fs); $fs.Close()
  } else { [IO.File]::WriteAllBytes($fileAbs, $ms.ToArray()) }
}

# ---------------------------------------------------------------- utilitarios de parse
function SplitOps([string]$s) {
  # separa por virgula fora de parenteses e aspas
  $out = New-Object System.Collections.Generic.List[string]
  $depth = 0; $q = $false; $cur = New-Object Text.StringBuilder
  foreach ($ch in $s.ToCharArray()) {
    if ($ch -eq "'") { $q = -not $q }
    if (-not $q) {
      if ($ch -eq '(') { $depth++ } elseif ($ch -eq ')') { $depth-- }
      elseif ($ch -eq ',' -and $depth -eq 0) { $out.Add($cur.ToString()); [void]$cur.Clear(); continue }
    }
    [void]$cur.Append($ch)
  }
  $out.Add($cur.ToString())
  return ,$out
}
function Unparen([string]$s) {
  $s = $s.Trim()
  if ($s.StartsWith('(') -and $s.EndsWith(')')) { $s = $s.Substring(1, $s.Length - 2) }
  return $s
}
function ParseKw([string]$operands) {
  $h = [ordered]@{}; $pos = New-Object System.Collections.Generic.List[string]
  if ($operands -eq $null -or $operands -eq '') { return @{ KW = $h; POS = $pos } }
  foreach ($o in (SplitOps $operands)) {
    $m = [regex]::Match($o, '^([A-Z0-9#@$.]+)=(.*)$')
    if ($m.Success) { $h[$m.Groups[1].Value] = $m.Groups[2].Value } else { $pos.Add($o) }
  }
  return @{ KW = $h; POS = $pos }
}
function SplitField([string]$line) {
  # devolve o primeiro token (respeitando aspas) e o resto
  $q = $false
  for ($i = 0; $i -lt $line.Length; $i++) {
    $c = $line[$i]
    if ($c -eq "'") { $q = -not $q }
    if ($c -eq ' ' -and -not $q) { return @($line.Substring(0, $i), $line.Substring($i).TrimStart()) }
  }
  return @($line, '')
}

# ---------------------------------------------------------------- simbolos
$SYM = @{ SYSUID = $USER }
function SubSym([string]$s, [ref]$err) {
  $rx = [regex]'(?<!&)&([A-Z#@$][A-Z0-9#@$]{0,7})(\.)?'
  return $rx.Replace($s, {
    param($m)
    $n = $m.Groups[1].Value
    if ($SYM.ContainsKey($n)) { return $SYM[$n] }
    $err.Value = "IEFC657I THE SYMBOL $n WAS NOT USED"
    return $m.Value
  })
}

# ======================================================================
#  LEITURA DO JCL
# ======================================================================
function ParseJcl([string[]]$lines) {
  $stmts = New-Object System.Collections.Generic.List[object]
  $jesjcl = New-Object System.Collections.Generic.List[string]
  $errors = New-Object System.Collections.Generic.List[string]
  $num = 0; $i = 0; $cur = $null; $expectCont = $false
  $ops = @('JOB', 'EXEC', 'DD', 'IF', 'ELSE', 'ENDIF', 'SET')
  while ($i -lt $lines.Count) {
    $raw = $lines[$i].TrimEnd("`r")
    $ln = $raw; if ($ln.Length -gt 72) { $ln = $ln.Substring(0, 72) }
    $i++
    if ($ln.StartsWith('//*')) { $jesjcl.Add(('{0,9} {1}' -f '', $ln)); continue }
    if (-not $ln.StartsWith('//')) {
      if ($ln.StartsWith('/*')) { $jesjcl.Add(('{0,9} {1}' -f '', $ln)); continue }
      $num++; $jesjcl.Add(('{0,9} {1}' -f $num, $ln))
      $errors.Add(('{0,8} SIM001E LINHA FORA DO PADRAO JCL: TODA LINHA DEVE COMECAR COM //' -f $num)); continue
    }
    $body = $ln.Substring(2)
    if ($expectCont) {
      if ($body.Length -gt 0 -and $body[0] -ne ' ') {
        $errors.Add(('{0,8} IEFC621I EXPECTED CONTINUATION NOT RECEIVED' -f $cur.NUM))
        $expectCont = $false
      } else {
        $t = $body.TrimStart()
        $col = $body.Length - $t.Length + 3
        $jesjcl.Add(('{0,9} {1}' -f '', $ln))
        if ($col -gt 16) { $errors.Add(('{0,8} IEFC618I OPERAND FIELD DOES NOT START IN COLUMN 4 THRU 16' -f $cur.NUM)) }
        $parts = SplitField $t
        $cur.OPER += $parts[0]
        $expectCont = $parts[0].EndsWith(',')
        continue
      }
    }
    $num++; $jesjcl.Add(('{0,9} {1}' -f $num, $ln))
    $name = ''
    if ($body.Length -gt 0 -and $body[0] -ne ' ') { $p = SplitField $body; $name = $p[0]; $rest = $p[1] } else { $rest = $body.TrimStart() }
    $p = SplitField $rest; $op = $p[0]; $oper = $p[1]
    if ($op -eq '') {
      if ($name -eq '') { continue }  # // sozinho = fim do job
      $errors.Add(('{0,8} IEFC605I UNIDENTIFIED OPERATION FIELD' -f $num)); continue
    }
    if ($ops -notcontains $op) { $errors.Add(('{0,8} IEFC605I UNIDENTIFIED OPERATION FIELD' -f $num)); continue }
    $s = [pscustomobject]@{ NUM = $num; NAME = $name; OP = $op; OPER = ''; DATA = $null }
    if ($op -eq 'IF') {
      $m = [regex]::Match($rest, '^IF\s*(.*?)\s*THEN\b')
      if (-not $m.Success) { $errors.Add(('{0,8} IEFC013I ERROR IN IF STATEMENT: THEN NOT FOUND' -f $num)); continue }
      $s.OPER = $m.Groups[1].Value
    } else {
      $q = SplitField $oper
      $s.OPER = $q[0]
      $expectCont = $q[0].EndsWith(',')
    }
    $stmts.Add($s); $cur = $s
    # dados in-stream
    if ($op -eq 'DD') {
      # o operando pode continuar nas linhas seguintes; DD * sempre vem sozinho ou com DLM/SYMBOLS na mesma linha
      $first = (SplitOps $s.OPER)[0]
      if ($first -eq '*' -or $first -eq 'DATA') {
        $dlm = '/*'
        if ($s.OPER -match 'DLM=([^,]+)') { $dlm = $matches[1].Trim("'") }
        $data = New-Object System.Collections.Generic.List[string]
        while ($i -lt $lines.Count) {
          $d = $lines[$i].TrimEnd("`r")
          if ($d.StartsWith($dlm)) { $jesjcl.Add(('{0,9} {1}' -f '', $d)); $i++; break }
          if ($first -eq '*' -and $d.StartsWith('//')) { break }
          $data.Add($d); $i++
        }
        $s.DATA = $data
        $expectCont = $false
      }
    }
  }
  if ($expectCont) { $errors.Add(('{0,8} IEFC621I EXPECTED CONTINUATION NOT RECEIVED' -f $cur.NUM)) }
  return @{ STMTS = $stmts; JESJCL = $jesjcl; ERR = $errors }
}

# ======================================================================
#  MONTAGEM DO JOB
# ======================================================================
function BuildJob($parsed) {
  $err = $parsed.ERR
  $st = $parsed.STMTS
  if ($st.Count -eq 0 -or $st[0].OP -ne 'JOB') { $err.Add('       1 IEFC607I JOB HAS NO JOB STATEMENT'); return $null }
  $job = @{ NAME = $st[0].NAME; STEPS = New-Object System.Collections.Generic.List[object]; JOBLIB = $null }
  if ($job.NAME -eq '') { $err.Add('       1 IEFC607I JOB HAS NO JOB STATEMENT'); return $null }
  $ifStack = New-Object System.Collections.Generic.List[object]
  $step = $null
  foreach ($s in $st) {
    $e = $null
    if ($s.OP -ne 'JOB') { $s.OPER = SubSym $s.OPER ([ref]$e) }
    if ($e) { $err.Add(('{0,8} {1}' -f $s.NUM, $e)) }
    switch ($s.OP) {
      'JOB' { }
      'SET' {
        foreach ($o in (SplitOps $s.OPER)) { if ($o -match '^([A-Z#@$][A-Z0-9#@$]{0,7})=(.*)$') { $SYM[$matches[1]] = $matches[2].Trim("'") } }
      }
      'IF' { $ifStack.Add(@{ EXPR = $s.OPER; BRANCH = 'THEN'; NUM = $s.NUM }) }
      'ELSE' { if ($ifStack.Count -eq 0) { $err.Add(('{0,8} IEFC019I MISPLACED ELSE STATEMENT' -f $s.NUM)) } else { $ifStack[$ifStack.Count - 1].BRANCH = 'ELSE' } }
      'ENDIF' { if ($ifStack.Count -eq 0) { $err.Add(('{0,8} IEFC019I MISPLACED ENDIF STATEMENT' -f $s.NUM)) } else { $ifStack.RemoveAt($ifStack.Count - 1) } }
      'EXEC' {
        $k = ParseKw $s.OPER
        $pgm = $null; $proc = $null
        if ($k.KW.Contains('PGM')) { $pgm = $k.KW['PGM'] }
        elseif ($k.KW.Contains('PROC')) { $proc = $k.KW['PROC'] }
        elseif ($k.POS.Count -gt 0) { $proc = $k.POS[0] }
        if (-not $pgm -and -not $proc) { $err.Add(('{0,8} IEFC009I KEYWORD PGM OR PROC IS MISSING' -f $s.NUM)) }
        if ($proc -and $proc -ne 'IGYWCL') { $err.Add(('{0,8} IEFC612I PROCEDURE {1} WAS NOT FOUND' -f $s.NUM, $proc)) }
        $conds = @(); foreach ($c in $ifStack) { $conds += @{ EXPR = $c.EXPR; BRANCH = $c.BRANCH } }
        $step = @{ NAME = $s.NAME; PGM = $pgm; PROC = $proc; COND = $k.KW['COND']; DDS = New-Object System.Collections.Generic.List[object]; IFS = $conds; NUM = $s.NUM }
        $job.STEPS.Add($step)
      }
      'DD' {
        $k = ParseKw $s.OPER
        $dd = @{ NAME = $s.NAME; KW = $k.KW; POS = $k.POS; DATA = $s.DATA; NUM = $s.NUM; PROCSTEP = '' }
        if ($dd.NAME.Contains('.')) { $x = $dd.NAME.Split('.'); $dd.PROCSTEP = $x[0]; $dd.NAME = $x[1] }
        if ($s.DATA -ne $null -and $s.OPER -match 'SYMBOLS=(JCLONLY|EXECSYS|CNVTSYS)') {
          $nd = New-Object System.Collections.Generic.List[string]
          foreach ($l in $s.DATA) { $e2 = $null; $nd.Add((SubSym $l ([ref]$e2))) }
          $dd.DATA = $nd
        }
        if ($step -eq $null) {
          if ($dd.NAME -eq 'JOBLIB') { $job.JOBLIB = $dd } else { $err.Add(('{0,8} IEFC011I MISPLACED DD STATEMENT' -f $s.NUM)) }
        } elseif ($dd.NAME -eq '' -and $step.DDS.Count -gt 0) {
          $prev = $step.DDS[$step.DDS.Count - 1]
          if (-not $prev.CONCAT) { $prev.CONCAT = New-Object System.Collections.Generic.List[object] }
          $prev.CONCAT.Add($dd)
        } else { $step.DDS.Add($dd) }
      }
    }
  }
  if ($ifStack.Count -gt 0) { $err.Add(('{0,8} IEFC018I MISSING ENDIF STATEMENT' -f $ifStack[0].NUM)) }
  return $job
}

# ======================================================================
#  EXECUCAO
# ======================================================================
$script:LOG = New-Object System.Collections.Generic.List[string]   # JESMSGLG
$script:SYS = New-Object System.Collections.Generic.List[string]   # JESYSMSG
$script:OUT = New-Object System.Collections.Generic.List[object]   # SYSOUTs
$script:RCS = @{}; $script:ABENDED = $false; $script:GDGTOP = @{}
function L([string]$t) { $script:LOG.Add((' {0} {1}  {2}' -f (Hora), $script:JOBID, $t)) }
function Y([string]$t) { $script:SYS.Add($t) }
function AddOut([string]$step, [string]$dd, [string]$text) { $script:OUT.Add(@{ STEP = $step; DD = $dd; TEXT = $text }) }

function ResolveDsn([string]$dsnRaw) {
  # devolve @{DSN; MEMBER; GDGBASE; GEN; TEMP}
  $r = @{ DSN = $dsnRaw; MEMBER = $null; GDGBASE = $null; GEN = $null; TEMP = $false }
  if ($dsnRaw.StartsWith('&&')) { $r.TEMP = $true; $r.DSN = 'SYS' + (Get-Date).ToString('yyMMdd') + '.T' + $script:JOBID + '.' + $dsnRaw.Substring(2); return $r }
  $m = [regex]::Match($dsnRaw, '^(.+)\(([+-]?\d+)\)$')
  if ($m.Success) {
    $base = $m.Groups[1].Value; $rel = [int]$m.Groups[2].Value
    if (-not (GdgIsBase $base)) { $r.ERROR = "IGD07001I GDG BASE $base NOT FOUND"; return $r }
    if (-not $script:GDGTOP.ContainsKey($base)) { $g = @(GdgGens $base); if ($g.Count -gt 0) { $script:GDGTOP[$base] = $g[-1] } else { $script:GDGTOP[$base] = 0 } }
    $n = $script:GDGTOP[$base] + $rel
    $r.GDGBASE = $base; $r.GEN = $rel
    if ($n -lt 1) { $r.ERROR = "IEF286I DSNAME $dsnRaw - GENERATION NOT FOUND"; return $r }
    $r.DSN = GdgName $base $n
    return $r
  }
  $m = [regex]::Match($dsnRaw, '^(.+)\(([A-Z#@$][A-Z0-9#@$]{0,7})\)$')
  if ($m.Success) { $r.DSN = $m.Groups[1].Value; $r.MEMBER = $m.Groups[2].Value }
  return $r
}
function DsnValid([string]$dsn) {
  if ($dsn.Length -gt 44) { return $false }
  foreach ($q in $dsn.Split('.')) { if ($q -notmatch '^[A-Z#@$][A-Z0-9#@$-]{0,7}$') { return $false } }
  return $true
}

function Allocate($step, [string]$jobname) {
  # devolve tabela DDNAME -> alocacao, ou $null se erro de JCL
  $tab = [ordered]@{}
  $all = @(); foreach ($d in $step.DDS) { $all += $d }
  foreach ($dd in $all) {
    $a = @{ DD = $dd.NAME; KIND = ''; PATH = $null; ISTEXT = $false; LRECL = 80; RECFM = 'FB'; DSN = $null; NEW = $false; DISP = @('SHR', '', ''); CONCAT = @(); PROCSTEP = $dd.PROCSTEP; MEMBER = $null }
    $kw = $dd.KW
    $isDummy = ($dd.POS -contains 'DUMMY') -or ($kw.Contains('DSN') -and $kw['DSN'] -eq 'NULLFILE') -or ($kw.Contains('DSNAME') -and $kw['DSNAME'] -eq 'NULLFILE')
    if ($kw.Contains('LRECL')) { $a.LRECL = [int]$kw['LRECL'] }
    if ($kw.Contains('RECFM')) { $a.RECFM = $kw['RECFM'] }
    if ($kw.Contains('DCB')) {
      foreach ($o in (SplitOps (Unparen $kw['DCB']))) { if ($o -match '^LRECL=(\d+)') { $a.LRECL = [int]$matches[1] }; if ($o -match '^RECFM=(\w+)') { $a.RECFM = $matches[1] } }
    }
    if ($dd.DATA -ne $null) {
      $f = J $Work ('{0}_{1}_{2}.txt' -f $script:JOBID, $step.NAME, $dd.NAME)
      [IO.File]::WriteAllLines($f, [string[]]$dd.DATA, $LAT)
      $a.KIND = 'INSTREAM'; $a.PATH = $f; $a.ISTEXT = $true
      Y ('IEF237I JES2 ALLOCATED TO {0}' -f $dd.NAME)
    } elseif ($isDummy) {
      $a.KIND = 'DUMMY'; Y ('IEF237I DMY  ALLOCATED TO {0}' -f $dd.NAME)
    } elseif ($kw.Contains('SYSOUT')) {
      $a.KIND = 'SYSOUT'; Y ('IEF237I JES2 ALLOCATED TO {0}' -f $dd.NAME)
    } elseif ($kw.Contains('DSN') -or $kw.Contains('DSNAME')) {
      $raw = $kw['DSN']; if (-not $raw) { $raw = $kw['DSNAME'] }
      $res = ResolveDsn $raw
      if ($res.ERROR) { Y $res.ERROR; return $null }
      if (-not $res.TEMP -and -not (DsnValid $res.DSN)) { Y ('IEFC624I INCORRECT USE OF PERIOD IN THE DSNAME FIELD: {0}' -f $res.DSN); return $null }
      $disp = @('NEW', '', '')
      if ($kw.Contains('DISP')) {
        $dv = SplitOps (Unparen $kw['DISP'])
        for ($k = 0; $k -lt 3 -and $k -lt $dv.Count; $k++) { if ($dv[$k] -ne '') { $disp[$k] = $dv[$k] } }
      }
      $a.DISP = $disp; $a.DSN = $res.DSN; $a.MEMBER = $res.MEMBER; $a.GDGBASE = $res.GDGBASE
      $exists = DsExists $res.DSN
      switch ($disp[0]) {
        'NEW' {
          if ($exists) { Y ('IGD17101I DATA SET {0} NOT DEFINED BECAUSE DUPLICATE NAME EXISTS IN CATALOG' -f $res.DSN); return $null }
          $pds = $false
          if ($kw.Contains('DSNTYPE') -and ($kw['DSNTYPE'] -match 'PDS|LIBRARY')) { $pds = $true }
          if ($kw.Contains('SPACE')) { $sp = SplitOps (Unparen $kw['SPACE']); if ($sp.Count -ge 2) { $q = SplitOps (Unparen $sp[1]); if ($q.Count -ge 3 -and $q[2] -ne '') { $pds = $true } } }
          if ($pds) { [void](New-Item -ItemType Directory -Path (DsFile $res.DSN)) } else { [IO.File]::WriteAllBytes((DsFile $res.DSN), (New-Object byte[] 0)) }
          SetAttr $res.DSN $a.RECFM $a.LRECL
          $a.NEW = $true
          Y ('IGD101I SMS ALLOCATED TO DDNAME ({0})' -f $dd.NAME); Y ('        DSN ({0})' -f $res.DSN)
        }
        'MOD' {
          if (-not $exists) { [IO.File]::WriteAllBytes((DsFile $res.DSN), (New-Object byte[] 0)); SetAttr $res.DSN $a.RECFM $a.LRECL; $a.NEW = $true }
          Y ('IGD103I SMS ALLOCATED TO DDNAME {0}' -f $dd.NAME)
        }
        default {
          if (-not $exists) { Y ('IEF212I {0} {1} {2} - DATA SET NOT FOUND' -f $jobname, $step.NAME, $dd.NAME); return $null }
          Y ('IGD103I SMS ALLOCATED TO DDNAME {0}' -f $dd.NAME)
        }
      }
      if (-not $a.NEW) { $at = GetAttr $res.DSN; $a.LRECL = $at.LRECL; $a.RECFM = $at.RECFM }
      $a.KIND = 'DS'
      $a.PATH = DsFile $res.DSN
      if (DsIsPds $res.DSN) {
        $a.KIND = 'PDS'
        if ($res.MEMBER) {
          $mf = MemberFile $res.DSN $res.MEMBER
          if ($mf) { $a.PATH = $mf } else { $a.PATH = J (DsFile $res.DSN) $res.MEMBER }
          $a.KIND = 'MEMBER'; $a.ISTEXT = $true
        }
      }
    } else {
      Y ('SIM002E DD {0} SEM DSN, SYSOUT, DUMMY OU *' -f $dd.NAME); return $null
    }
    if ($dd.CONCAT) {
      foreach ($c in $dd.CONCAT) {
        $raw = $c.KW['DSN']; if (-not $raw) { $raw = $c.KW['DSNAME'] }
        $res = ResolveDsn $raw
        if ($res.ERROR -or -not (DsExists $res.DSN)) { Y ('IEF212I {0} {1} {2} - DATA SET NOT FOUND' -f $jobname, $step.NAME, $dd.NAME); return $null }
        $a.CONCAT += (DsFile $res.DSN)
      }
    }
    $key = $dd.NAME; if ($dd.PROCSTEP) { $key = $dd.PROCSTEP + '.' + $dd.NAME }
    $tab[$key] = $a
  }
  return $tab
}

function Dispose($tab, [bool]$abend) {
  foreach ($a in $tab.Values) {
    if ($a.KIND -notin @('DS', 'PDS', 'MEMBER') -or -not $a.DSN) { continue }
    $d = $a.DISP
    $act = $d[1]; if ($abend -and $d[2] -ne '') { $act = $d[2] }
    if ($act -eq '') { if ($a.NEW) { $act = 'DELETE' } else { $act = 'KEEP' } }
    if ($a.DISP[0] -eq 'MOD' -and -not $a.NEW -and $act -eq '') { $act = 'KEEP' }
    switch ($act) {
      'DELETE' {
        $p = DsFile $a.DSN
        if (Test-Path -LiteralPath $p) { Remove-Item -LiteralPath $p -Recurse -Force }
        DelAttr $a.DSN
        Y ('IEF285I   {0,-44} DELETED' -f $a.DSN)
      }
      'CATLG' {
        Y ('IEF285I   {0,-44} CATALOGED' -f $a.DSN)
        if ($a.GDGBASE) {
          $info = GdgInfo $a.GDGBASE; $g = @(GdgGens $a.GDGBASE)
          while ($g.Count -gt $info.LIMIT) {
            $old = GdgName $a.GDGBASE $g[0]
            Remove-Item -LiteralPath (DsFile $old) -Force; DelAttr $old
            Y ('IGD07001I GDG ROLL OFF: {0} SCRATCHED' -f $old)
            $g = @(GdgGens $a.GDGBASE)
          }
        }
      }
      default { Y ('IEF285I   {0,-44} KEPT' -f $a.DSN) }
    }
  }
}

# ---------------------------------------------------------------- condicoes
function RcOf([string]$stepName) {
  if ($script:RCS.ContainsKey($stepName)) { return $script:RCS[$stepName] }
  return $null
}
function EvalIf([string]$expr) {
  $e = $expr.Trim()
  $e = $e -replace '\bAND\b', '&' -replace '\bOR\b', '|' -replace '\bNOT\b', '¬'
  $toks = [regex]::Matches($e, '[A-Z0-9#@$]+\.[A-Z0-9#@$.]+|[A-Z0-9#@$]+|<=|>=|¬=|<|>|=|&|\||¬|\(|\)')
  $t = @(); foreach ($m in $toks) { $t += $m.Value }
  $script:ti = 0
  function Peek { if ($script:ti -lt $t.Count) { return $t[$script:ti] } return $null }
  function Next { $v = Peek; $script:ti++; return $v }
  function Prim {
    $v = Next
    if ($v -eq '(') { $r = OrE; [void](Next); return $r }
    if ($v -eq '¬') { return (-not (Prim)) }
    # termo: STEP.RC | RC | ABEND | STEP.ABEND | numero
    $left = $v
    $op = Peek
    if ($op -in @('=', '<', '>', '<=', '>=', '¬=', 'EQ', 'NE', 'GT', 'LT', 'GE', 'LE')) {
      [void](Next); $right = Next
      $lv = TermVal $left; $rv = TermVal $right
      if ($lv -eq $null -or $rv -eq $null) { return $false }
      switch ($op) {
        { $_ -in '=', 'EQ' } { return ($lv -eq $rv) }
        { $_ -in '¬=', 'NE' } { return ($lv -ne $rv) }
        { $_ -in '>', 'GT' } { return ($lv -gt $rv) }
        { $_ -in '<', 'LT' } { return ($lv -lt $rv) }
        { $_ -in '>=', 'GE' } { return ($lv -ge $rv) }
        { $_ -in '<=', 'LE' } { return ($lv -le $rv) }
      }
    }
    if ($left -eq 'ABEND') { return $script:ABENDED }
    if ($left -match '^(.+)\.ABEND$') { $r = RcOf $matches[1]; return ($r -is [string]) }
    return $false
  }
  function AndE { $r = Prim; while ((Peek) -eq '&') { [void](Next); $b = Prim; $r = $r -and $b }; return $r }
  function OrE { $r = AndE; while ((Peek) -eq '|') { [void](Next); $b = AndE; $r = $r -or $b }; return $r }
  return (OrE)
}
function TermVal([string]$x) {
  if ($x -match '^\d+$') { return [int]$x }
  if ($x -eq 'RC') { $mx = 0; foreach ($v in $script:RCS.Values) { if ($v -is [int] -and $v -gt $mx) { $mx = $v } }; return $mx }
  if ($x -match '^(.+)\.RC$') { $r = RcOf $matches[1]; if ($r -is [int]) { return $r }; return $null }
  return $null
}
function CondBypass([string]$cond) {
  # COND=(codigo,operador[,step]) ou ((..),(..)) ; EVEN/ONLY
  if (-not $cond) { return $false }
  $c = Unparen $cond
  $tests = @()
  if ($c.StartsWith('(')) { foreach ($x in (SplitOps $c)) { $tests += ,(SplitOps (Unparen $x)) } } else { $tests += ,(SplitOps $c) }
  foreach ($t in $tests) {
    if ($t.Count -lt 2) { continue }
    $code = [int]$t[0]; $op = $t[1]
    $rcs = @()
    if ($t.Count -ge 3) { $r = RcOf $t[2]; if ($r -is [int]) { $rcs += $r } } else { foreach ($v in $script:RCS.Values) { if ($v -is [int]) { $rcs += $v } } }
    foreach ($rc in $rcs) {
      $hit = switch ($op) { 'GT' { $code -gt $rc } 'GE' { $code -ge $rc } 'EQ' { $code -eq $rc } 'LT' { $code -lt $rc } 'LE' { $code -le $rc } 'NE' { $code -ne $rc } default { $false } }
      if ($hit) { return $true }
    }
  }
  return $false
}

# ---------------------------------------------------------------- processos
function RunProc([string]$exe, [string]$args0, $envs) {
  $psi = New-Object Diagnostics.ProcessStartInfo
  if ($CFG.RUNNER) { $psi.FileName = $CFG.RUNNER; $psi.Arguments = ('"{0}" {1}' -f $exe, $args0) } else { $psi.FileName = $exe; $psi.Arguments = $args0 }
  $psi.UseShellExecute = $false; $psi.RedirectStandardOutput = $true; $psi.RedirectStandardError = $true
  $psi.WorkingDirectory = $Raiz; $psi.CreateNoWindow = $true
  if ($envs) { foreach ($k in $envs.Keys) { $psi.EnvironmentVariables[$k] = $envs[$k] } }
  $p = [Diagnostics.Process]::Start($psi)
  $o = $p.StandardOutput.ReadToEndAsync(); $e = $p.StandardError.ReadToEndAsync()
  $p.WaitForExit()
  return @{ RC = $p.ExitCode; OUT = $o.Result; ERR = $e.Result }
}

# ---------------------------------------------------------------- programas do sistema
function Pgm-IEBGENER($tab, $stepName) {
  $pr = New-Object Text.StringBuilder
  [void]$pr.AppendLine('                    DATA SET UTILITY - GENERATE')
  if (-not $tab.Contains('SYSUT1') -or -not $tab.Contains('SYSUT2')) { [void]$pr.AppendLine('IEB303I INVALID CONTROL STATEMENT OR DD MISSING (SYSUT1/SYSUT2)'); AddOut $stepName 'SYSPRINT' $pr.ToString(); return 12 }
  $i = $tab['SYSUT1']; $o = $tab['SYSUT2']
  if ($o.NEW -and -not $o.DSN) { }
  if (-not $o.NEW -and $i.LRECL -ne $o.LRECL -and $o.KIND -eq 'DS') { [void]$pr.AppendLine('IEB311I CONFLICTING DCB PARAMETERS'); AddOut $stepName 'SYSPRINT' $pr.ToString(); return 12 }
  if ($o.NEW -and $o.DSN) {
    # saida nova herda o LRECL da entrada quando o DD nao informa
    $dd = $null
    if ($o.LRECL -eq 80 -and $i.LRECL -ne 80) { $o.LRECL = $i.LRECL; SetAttr $o.DSN $o.RECFM $o.LRECL }
  }
  if ($o.LRECL -ne $i.LRECL -and $i.KIND -ne 'INSTREAM') { [void]$pr.AppendLine('IEB311I CONFLICTING DCB PARAMETERS'); AddOut $stepName 'SYSPRINT' $pr.ToString(); return 12 }
  $recs = New-Object System.Collections.Generic.List[byte[]]
  if ($i.KIND -ne 'DUMMY') { foreach ($r in (ReadRecords $i.PATH $i.LRECL $i.ISTEXT)) { $recs.Add($r) } }
  foreach ($cp in $i.CONCAT) { foreach ($r in (ReadRecords $cp $i.LRECL $false)) { $recs.Add($r) } }
  if ($o.KIND -ne 'DUMMY' -and $o.KIND -ne 'SYSOUT') { WriteRecords $o.PATH $recs $o.LRECL $o.ISTEXT ($o.DISP[0] -eq 'MOD') }
  if ($o.KIND -eq 'SYSOUT') { $t = ''; foreach ($r in $recs) { $t += $LAT.GetString($r).TrimEnd() + "`r`n" }; AddOut $stepName 'SYSUT2' $t }
  [void]$pr.AppendLine('PROCESSING ENDED AT EOD')
  [void]$pr.AppendLine(('SIM010I {0} REGISTROS COPIADOS' -f $recs.Count))
  AddOut $stepName 'SYSPRINT' $pr.ToString()
  return 0
}

function Pgm-SORT($tab, $stepName) {
  $pr = New-Object Text.StringBuilder
  foreach ($n in @('SORTIN', 'SORTOUT', 'SYSIN')) { if (-not $tab.Contains($n)) { [void]$pr.AppendLine("ICE057A  SORTIN, SORTOUT OU SYSIN AUSENTE: $n"); AddOut $stepName 'SYSOUT' $pr.ToString(); return 16 } }
  $i = $tab['SORTIN']; $o = $tab['SORTOUT']
  $ctl = @()
  foreach ($l in [IO.File]::ReadAllLines($tab['SYSIN'].PATH, $LAT)) { if ($l.Trim() -ne '' -and -not $l.TrimStart().StartsWith('*')) { $ctl += (' ' + $l.Trim()) } }
  $inFiles = @($i.PATH) + @($i.CONCAT)
  $take = J $Work ('{0}_{1}.take' -f $script:JOBID, $stepName)
  $lines = @()
  foreach ($f in $inFiles) { $lines += (' USE {0} RECORD F,{1} ORG SQ' -f (Rel $f), $i.LRECL) }
  if ($o.NEW -and $o.LRECL -eq 80 -and $i.LRECL -ne 80) { $o.LRECL = $i.LRECL; SetAttr $o.DSN $o.RECFM $o.LRECL }
  $lines += (' GIVE {0} RECORD F,{1} ORG SQ' -f (Rel $o.PATH), $o.LRECL)
  $lines += $ctl
  [IO.File]::WriteAllLines($take, [string[]]$lines, $LAT)
  [void]$pr.AppendLine('SIM020I SORT EXECUTADO PELO GCSORT (COMPATIVEL COM DFSORT)')
  foreach ($c in $ctl) { [void]$pr.AppendLine(' ' + $c.Trim()) }
  $r = RunProc $CFG.GCSORT ('TAKE "{0}"' -f (Rel $take)) $null
  [void]$pr.AppendLine($r.OUT); if ($r.ERR) { [void]$pr.AppendLine($r.ERR) }
  $ok = ($r.OUT -match 'Sort OK|Merge OK|Copy OK') -and $r.RC -eq 0
  if ($ok) {
    $n = 0; if (Test-Path -LiteralPath $o.PATH) { $n = [int]((Get-Item -LiteralPath $o.PATH).Length / $o.LRECL) }
    [void]$pr.AppendLine(('ICE054I 0 RECORDS - OUT: {0}' -f $n))
    [void]$pr.AppendLine('ICE052I 0 END OF DFSORT')
    AddOut $stepName 'SYSOUT' $pr.ToString(); return 0
  }
  [void]$pr.AppendLine('ICE052I 3 END OF DFSORT - ERRO NAS INSTRUCOES DE CONTROLE OU NOS ARQUIVOS')
  AddOut $stepName 'SYSOUT' $pr.ToString(); return 16
}

function Pgm-IDCAMS($tab, $stepName) {
  $pr = New-Object Text.StringBuilder
  [void]$pr.AppendLine(('IDCAMS  SYSTEM SERVICES                                           TIME: {0}' -f (Get-Date).ToString('HH:mm:ss')))
  if (-not $tab.Contains('SYSIN')) { [void]$pr.AppendLine('IDC3203I ITEM SYSIN DOES NOT ADHERE TO RESTRICTIONS'); AddOut $stepName 'SYSPRINT' $pr.ToString(); return 12 }
  $src = [IO.File]::ReadAllLines($tab['SYSIN'].PATH, $LAT)
  # junta continuacoes (- ou +) e remove comentarios
  $cmds = @(); $buf = ''; $echo = @()
  foreach ($l in $src) {
    $x = ($l -replace '/\*.*?\*/', '')
    if ($x.Length -gt 72) { $x = $x.Substring(0, 72) }
    $x = $x.TrimEnd(); if ($x.Trim() -eq '') { continue }
    $echo += $l.TrimEnd()
    if ($x.EndsWith('-') -or $x.EndsWith('+')) { $buf += ' ' + $x.Substring(0, $x.Length - 1) }
    else { $buf += ' ' + $x; $cmds += ,@($buf.Trim(), $echo); $buf = ''; $echo = @() }
  }
  $maxcc = 0; $lastcc = 0
  foreach ($cmdPair in $cmds) {
    $cmd = $cmdPair[0].ToUpper(); $ech = $cmdPair[1]
    [void]$pr.AppendLine('0')
    foreach ($e in $ech) { [void]$pr.AppendLine(' ' + $e) }
    $cc = 0
    if ($cmd -match '^SET\s+(MAXCC|LASTCC)\s*=\s*(\d+)') {
      if ($matches[1] -eq 'MAXCC') { $maxcc = [int]$matches[2] } else { $lastcc = [int]$matches[2] }
      continue
    }
    if ($cmd -match '^(DEFINE|DEF)\s+(GDG|GENERATIONDATAGROUP)\s*\((.*)\)\s*$') {
      $body = $matches[3]
      $name = if ($body -match 'NAME\s*\(\s*([^)\s]+)\s*\)') { $matches[1] } else { '' }
      $limit = if ($body -match 'LIMIT\s*\(\s*(\d+)\s*\)') { [int]$matches[1] } else { 0 }
      if (-not $name -or $limit -lt 1) { [void]$pr.AppendLine('0IDC3211I KEYWORD NAME OU LIMIT AUSENTE OU INVALIDA'); $cc = 12 }
      elseif ((GdgIsBase $name) -or (DsExists $name)) {
        [void]$pr.AppendLine('0IDC3013I DUPLICATE DATA SET NAME')
        [void]$pr.AppendLine(' IDC3009I ** VSAM CATALOG RETURN CODE IS 8 - REASON CODE IS IGG0CLEH-38'); $cc = 12
      } else {
        $scr = if ($body -match '\bNOSCRATCH\b|\bNSCR\b') { 'NOSCRATCH' } else { 'SCRATCH' }
        [IO.File]::WriteAllText((GdgBaseFile $name), "LIMIT=$limit $scr")
        $cc = 0
      }
    }
    elseif ($cmd -match '^(DELETE|DEL)\s+\(?\s*([A-Z0-9#@$.()+-]+?)\s*\)?(\s+.*)?$') {
      $name = $matches[2]; $opts = $matches[3]
      if (GdgIsBase $name) {
        $g = @(GdgGens $name)
        if ($g.Count -gt 0 -and $opts -notmatch 'FORCE') { [void]$pr.AppendLine("0IDC3009I ** VSAM CATALOG RETURN CODE IS 12 - GDG $name AINDA TEM GERACOES (USE FORCE)"); $cc = 8 }
        else {
          foreach ($n in $g) { $gn = GdgName $name $n; Remove-Item -LiteralPath (DsFile $gn) -Force; DelAttr $gn; [void]$pr.AppendLine("0IDC0550I ENTRY (A) $gn DELETED") }
          Remove-Item -LiteralPath (GdgBaseFile $name) -Force
          [void]$pr.AppendLine("0IDC0550I ENTRY (B) $name DELETED"); $cc = 0
        }
      } elseif (DsExists $name) {
        Remove-Item -LiteralPath (DsFile $name) -Recurse -Force; DelAttr $name
        [void]$pr.AppendLine("0IDC0550I ENTRY (A) $name DELETED"); $cc = 0
      } else {
        [void]$pr.AppendLine("0IDC3012I ENTRY $name NOT FOUND")
        [void]$pr.AppendLine(' IDC3009I ** VSAM CATALOG RETURN CODE IS 8 - REASON CODE IS IGG0CLEG-42'); $cc = 8
      }
    }
    elseif ($cmd -match '^REPRO\s+(.*)$') {
      $o = $matches[1]
      $inA = $null; $outA = $null
      if ($o -match '(INFILE|IFILE)\s*\(\s*(\w+)\s*\)') { if ($tab.Contains($matches[2])) { $inA = $tab[$matches[2]] } }
      if ($o -match '(INDATASET|IDS)\s*\(\s*([^)\s]+)\s*\)') { $n = $matches[2]; if (DsExists $n) { $at = GetAttr $n; $inA = @{ PATH = DsFile $n; LRECL = $at.LRECL; ISTEXT = $false } } }
      if ($o -match '(OUTFILE|OFILE)\s*\(\s*(\w+)\s*\)') { if ($tab.Contains($matches[2])) { $outA = $tab[$matches[2]] } }
      if ($o -match '(OUTDATASET|ODS)\s*\(\s*([^)\s]+)\s*\)') { $n = $matches[2]; if (DsExists $n) { $at = GetAttr $n; $outA = @{ PATH = DsFile $n; LRECL = $at.LRECL; ISTEXT = $false } } }
      if (-not $inA -or -not $outA) { [void]$pr.AppendLine('0IDC3300I ERROR OPENING DATA SET'); $cc = 12 }
      else {
        $recs = ReadRecords $inA.PATH $inA.LRECL $inA.ISTEXT
        WriteRecords $outA.PATH $recs $outA.LRECL $false $false
        [void]$pr.AppendLine(('0IDC0005I NUMBER OF RECORDS PROCESSED WAS {0}' -f $recs.Count)); $cc = 0
      }
    }
    elseif ($cmd -match '^(LISTCAT|LISTC)\b(.*)$') {
      $o = $matches[2]; $pat = ''
      if ($o -match '(ENTRIES|ENT|LEVEL|LVL)\s*\(\s*([^)\s]+)\s*\)') { $pat = $matches[2] }
      $names = @()
      foreach ($f in (Get-ChildItem -LiteralPath $Cat)) { if ($f.Name -ne '.atrib') { $names += $f.Name } }
      foreach ($f in (Get-ChildItem -LiteralPath $Atr -Filter '*.GDGBASE')) { $names += ('GDG BASE ' + $f.BaseName) }
      $n = 0
      foreach ($x in ($names | Sort-Object)) { if ($pat -eq '' -or $x.Contains($pat)) { [void]$pr.AppendLine('0' + $x); $n++ } }
      [void]$pr.AppendLine(('0IDC0001I {0} ENTRADAS LISTADAS' -f $n)); $cc = 0
    }
    else { [void]$pr.AppendLine('0IDC3211I KEYWORD OU COMANDO INVALIDO'); $cc = 12 }
    if ($cc -eq 0) { [void]$pr.AppendLine('0IDC0001I FUNCTION COMPLETED, HIGHEST CONDITION CODE WAS 0') }
    else { [void]$pr.AppendLine(('0IDC3003I FUNCTION TERMINATED. CONDITION CODE IS {0}' -f $cc)) }
    $lastcc = $cc; if ($cc -gt $maxcc) { $maxcc = $cc }
  }
  [void]$pr.AppendLine('0')
  [void]$pr.AppendLine(('0IDC0002I IDCAMS PROCESSING COMPLETE. MAXIMUM CONDITION CODE WAS {0}' -f $maxcc))
  AddOut $stepName 'SYSPRINT' $pr.ToString()
  return $maxcc
}

function LoadLibs($tab, $job) {
  $libs = @()
  if ($tab.Contains('STEPLIB')) { $libs += $tab['STEPLIB'].PATH; $libs += $tab['STEPLIB'].CONCAT }
  elseif ($job.JOBLIB) { $r = ResolveDsn $job.JOBLIB.KW['DSN']; if (DsExists $r.DSN) { $libs += (DsFile $r.DSN) } }
  return $libs
}

function Pgm-User([string]$pgm, $tab, $stepName, $job) {
  $libs = LoadLibs $tab $job
  $exe = $null
  foreach ($l in $libs) { $c = J $l ($pgm + $CFG.EXE); if (Test-Path -LiteralPath $c) { $exe = $c; break } }
  if (-not $exe) { Y ('CSV003I REQUESTED MODULE {0} NOT FOUND' -f $pgm); return 'S806' }
  $envs = @{}
  foreach ($k in $tab.Keys) {
    $a = $tab[$k]
    $v = $null
    switch ($a.KIND) { 'DUMMY' { $v = 'NUL' } 'SYSOUT' { $v = $null } default { $v = Rel $a.PATH } }
    if ($v) { $envs['DD_' + $a.DD] = $v }
  }
  $envs['COB_LIBRARY_PATH'] = (($libs | ForEach-Object { Rel $_ }) -join [IO.Path]::PathSeparator)
  $envs['COB_SET_TRACE'] = 'N'
  $r = RunProc $exe '' $envs
  $outDd = 'SYSOUT'
  AddOut $stepName $outDd $r.OUT
  if ($r.ERR -match 'libcob: (error|erro)|attempt to reference|signal SIG') {
    $txt = $r.ERR
    if ($tab.Contains('CEEDUMP')) { AddOut $stepName 'CEEDUMP' $txt } else { AddOut $stepName 'SYSOUT-ERRO' $txt }
    if ($txt -match 'not numeric|nao numerico|incompatible|data exception') { return 'S0C7' }
    if ($txt -match 'SIGSEGV|invalid memory|attempt to reference') { return 'S0C4' }
    return 'U4038'
  }
  if ($r.ERR) { AddOut $stepName 'SYSOUT-ERRO' $r.ERR }
  $rc = $r.RC
  if ($rc -lt 0 -or $rc -gt 4095) { return 'S0C4' }
  return $rc
}

function IsMainProgram([string]$src) {
  $t = [IO.File]::ReadAllText($src, $LAT)
  return -not ($t -match '(?im)^.{6}[^*\/].*PROCEDURE\s+DIVISION\s+USING')
}

function Proc-IGYWCL($step, $tab, $job) {
  # step COBOL: compila; step LKED: gera o executavel na biblioteca LOAD
  $res = @()
  $src = $tab['COBOL.SYSIN']; $lib = $tab['COBOL.SYSLIB']; $lmod = $tab['LKED.SYSLMOD']
  if (-not $src -or -not $lmod) { Y 'SIM030E IGYWCL PRECISA DE COBOL.SYSIN E LKED.SYSLMOD'; return @(@('COBOL', 12), @('LKED', 'FLUSH')) }
  $mem = $lmod.MEMBER; if (-not $mem) { $mem = $src.MEMBER }
  $tmpSrc = J $Work ('{0}.cbl' -f $mem)
  Copy-Item -LiteralPath $src.PATH -Destination $tmpSrc -Force
  $obj = J $Work ('{0}.o' -f $mem); $lst = J $Work ('{0}.lst' -f $mem)
  foreach ($f in @($obj, $lst)) { if (Test-Path -LiteralPath $f) { Remove-Item -LiteralPath $f -Force } }
  $main = IsMainProgram $tmpSrc
  $inc = ''
  if ($lib) { $inc = '-I "{0}"' -f (Rel $lib.PATH); foreach ($c in $lib.CONCAT) { $inc += ' -I "{0}"' -f (Rel $c) } }
  $xflag = ''; if ($main) { $xflag = '-x' }
  $a = ('-c {0} -std=ibm {1} -t "{2}" -o "{3}" "{4}"' -f $xflag, $inc, (Rel $lst), (Rel $obj), (Rel $tmpSrc))
  $r = RunProc $CFG.COBC $a $null
  $listing = ''; if (Test-Path -LiteralPath $lst) { $listing = [IO.File]::ReadAllText($lst, $LAT) }
  $msgs = ($r.OUT + $r.ERR).Trim()
  $nome = '{0}({1})' -f $src.DSN, $mem
  $msgs = $msgs.Replace((Rel $tmpSrc), $nome)
  $listing = $listing.Replace((Rel $tmpSrc), $nome)
  $rc = 0
  if ($r.RC -ne 0 -or -not (Test-Path -LiteralPath $obj)) { $rc = 12 } elseif ($msgs -match '(?i)warning|aviso') { $rc = 4 }
  AddOut ($step.NAME + '.COBOL') 'SYSPRINT' (("SIM031I COMPILACAO COM GNUCOBOL (-std=ibm), PROGRAMA {0} ({1})`r`n" -f $mem, $(if ($main) { 'PRINCIPAL' } else { 'SUBPROGRAMA' })) + "`r`n" + $msgs + "`r`n`r`n" + $listing)
  $res += ,@('COBOL', $rc)
  if ($rc -gt 8) { $res += ,@('LKED', 'FLUSH'); return $res }
  $dir = Split-Path -Parent $lmod.PATH
  if ($lmod.KIND -eq 'PDS') { $dir = $lmod.PATH }
  if ($main) { $out = J $dir ($mem + $CFG.EXE); $a2 = ('-x -o "{0}" "{1}"' -f (Rel $out), (Rel $obj)) }
  else { $out = J $dir ($mem + $CFG.MOD); $a2 = ('-m -o "{0}" "{1}"' -f (Rel $out), (Rel $obj)) }
  # o PATH do membro pode ter sido criado como arquivo vazio pela alocacao: remove
  if ((Test-Path -LiteralPath $lmod.PATH -PathType Leaf) -and (Get-Item -LiteralPath $lmod.PATH).Length -eq 0) { Remove-Item -LiteralPath $lmod.PATH -Force }
  $r2 = RunProc $CFG.COBC $a2 $null
  $rc2 = 0; if ($r2.RC -ne 0 -or -not (Test-Path -LiteralPath $out)) { $rc2 = 12 }
  AddOut ($step.NAME + '.LKED') 'SYSPRINT' (("SIM032I LIGACAO: {0}`r`n" -f (Rel $out)) + ($r2.OUT + $r2.ERR))
  $res += ,@('LKED', $rc2)
  return $res
}

# ---------------------------------------------------------------- execucao do job
function RunJob($job, $parsed, [string]$jclText) {
  $n = 1; $nf = J $Sis 'jobnum.txt'
  if (Test-Path $nf) { $n = [int]([IO.File]::ReadAllText($nf).Trim()) + 1 }
  [IO.File]::WriteAllText($nf, "$n")
  $script:JOBID = 'JOB{0:D5}' -f $n
  $jn = $job.NAME
  $dir = J $Spool ('{0}_{1}' -f $jn, $script:JOBID)
  [void](New-Item -ItemType Directory -Path $dir -Force)
  $hdr = ' {0}  ----  {1}  ----' -f (Hora), (Get-Date).ToString('dddd, dd MMM yyyy', [Globalization.CultureInfo]::InvariantCulture).ToUpper()
  $script:LOG.Add('                    J E S 2   J O B   L O G   --   M A I N F R A M E   L O C A L')
  $script:LOG.Add($hdr)
  $result = ''
  $table = New-Object System.Collections.Generic.List[string]
  if ($parsed.ERR.Count -gt 0) {
    L ('IEFC452I {0} - JOB NOT RUN - JCL ERROR' -f $jn)
    $script:SYS.Add(' STMT NO. MESSAGE')
    foreach ($e in $parsed.ERR) { $script:SYS.Add($e) }
    $result = 'JCL ERROR'
  } else {
    L ('IRR010I  USERID {0,-8} IS ASSIGNED TO THIS JOB.' -f $USER)
    L ('$HASP373 {0,-8} STARTED - INIT 1    - CLASS A        - SYS LOCAL' -f $jn)
    $table.Add('-STEPNAME PROCSTEP    RC')
    $maxrc = 0; $abendCode = $null; $jclFail = $false
    foreach ($st in $job.STEPS) {
      $sn = $st.NAME
      if ($jclFail) { break }
      $run = $true; $why = ''
      foreach ($c in $st.IFS) {
        $v = EvalIf $c.EXPR
        if ($c.BRANCH -eq 'ELSE') { $v = -not $v }
        if (-not $v) { $run = $false; $why = 'IF' }
      }
      $condTxt = [string]$st.COND
      if ($run -and $script:ABENDED -and $condTxt -notmatch 'EVEN|ONLY' -and -not ($st.IFS | Where-Object { $_.EXPR -match 'ABEND' })) { $run = $false; $why = 'ABEND' }
      if ($run -and $condTxt -match 'ONLY' -and -not $script:ABENDED) { $run = $false; $why = 'COND' }
      if ($run -and (CondBypass ($condTxt -replace ',?(EVEN|ONLY)', ''))) { $run = $false; $why = 'COND' }
      if (-not $run) {
        switch ($why) {
          'IF' { Y ('IEF206I {0} {1} - STEP WAS NOT EXECUTED BECAUSE OF CONDITIONAL STATEMENT (IF)' -f $jn, $sn) }
          'COND' { Y ('IEF202I {0} {1} - STEP WAS NOT RUN BECAUSE OF CONDITION CODES' -f $jn, $sn) }
          default { Y ('IEF272I {0} {1} - STEP WAS NOT EXECUTED.' -f $jn, $sn) }
        }
        $script:RCS[$sn] = 'FLUSH'
        $table.Add(('-{0,-8} {1,-8} FLUSH' -f $sn, ''))
        continue
      }
      $tab = Allocate $st $jn
      if ($tab -eq $null) {
        Y ('IEF272I {0} {1} - STEP WAS NOT EXECUTED.' -f $jn, $sn)
        $jclFail = $true; $table.Add(('-{0,-8} {1,-8} FLUSH' -f $sn, '')); break
      }
      if ($st.PROC -eq 'IGYWCL') {
        $parts = Proc-IGYWCL $st $tab $job
        $worst = 0
        foreach ($pp in $parts) {
          $ps = $pp[0]; $rc = $pp[1]
          if ($rc -is [int]) {
            Y ('IEF142I {0} {1} {2} - STEP WAS EXECUTED - COND CODE {3:D4}' -f $jn, $sn, $ps, $rc)
            $table.Add(('-{0,-8} {1,-8} {2,5:D2}' -f $sn, $ps, $rc)); if ($rc -gt $worst) { $worst = $rc }
            $script:RCS["$sn.$ps"] = $rc
          } else {
            Y ('IEF202I {0} {1} {2} - STEP WAS NOT RUN BECAUSE OF CONDITION CODES' -f $jn, $sn, $ps)
            $table.Add(('-{0,-8} {1,-8} FLUSH' -f $sn, $ps)); $script:RCS["$sn.$ps"] = 'FLUSH'
          }
        }
        $script:RCS[$sn] = $worst; if ($worst -gt $maxrc) { $maxrc = $worst }
        Dispose $tab $false
        continue
      }
      $pgm = $st.PGM
      switch ($pgm) {
        'IEFBR14' { $rc = 0 }
        'IEBGENER' { $rc = Pgm-IEBGENER $tab $sn }
        'ICEGENER' { $rc = Pgm-IEBGENER $tab $sn }
        { $_ -in 'SORT', 'ICEMAN', 'DFSORT', 'SYNCSORT' } { $rc = Pgm-SORT $tab $sn }
        'IDCAMS' { $rc = Pgm-IDCAMS $tab $sn }
        { $_ -in 'IKJEFT01', 'IKJEFT1B', 'DSNTEP2', 'DSNTIAUL' } { Y ('SIM040E O PROGRAMA {0} (TSO/DB2) NAO EXISTE NESTE AMBIENTE LOCAL' -f $pgm); $rc = 'S806' }
        default { $rc = Pgm-User $pgm $tab $sn $job }
      }
      if ($rc -is [int]) {
        Y ('IEF142I {0} {1} - STEP WAS EXECUTED - COND CODE {2:D4}' -f $jn, $sn, $rc)
        $table.Add(('-{0,-8} {1,-8} {2,5:D2}' -f $sn, '', $rc))
        $script:RCS[$sn] = $rc; if ($rc -gt $maxrc) { $maxrc = $rc }
        Dispose $tab $false
      } else {
        $abendCode = $rc
        $u = if ($rc.StartsWith('U')) { 'S000 ' + $rc } else { $rc + ' U0000' }
        Y ('IEF450I {0} {1} - ABEND={2} REASON=00000000' -f $jn, $sn, $u)
        $table.Add(('-{0,-8} {1,-8} *{2}' -f $sn, '', $rc))
        $script:RCS[$sn] = $rc; $script:ABENDED = $true
        Dispose $tab $true
      }
    }
    foreach ($t in $table) { L $t }
    if ($jclFail) { L ('IEF453I {0} - JOB FAILED - JCL ERROR' -f $jn); $result = 'JCL ERROR' }
    elseif ($abendCode) { $result = 'ABEND=' + $abendCode }
    else { $result = 'RC={0:D4}' -f $maxrc }
    L ('$HASP395 {0,-8} ENDED - {1}' -f $jn, $result)
  }
  # grava o spool
  $k = 1
  [IO.File]::WriteAllLines((J $dir ('{0:D2}-JESMSGLG.log' -f $k)), [string[]]$script:LOG, $LAT); $k++
  [IO.File]::WriteAllLines((J $dir ('{0:D2}-JESJCL.log' -f $k)), [string[]]$parsed.JESJCL, $LAT); $k++
  [IO.File]::WriteAllLines((J $dir ('{0:D2}-JESYSMSG.log' -f $k)), [string[]]$script:SYS, $LAT); $k++
  foreach ($o in $script:OUT) {
    if ($o.TEXT -eq $null -or $o.TEXT.Trim() -eq '') { continue }
    [IO.File]::WriteAllText((J $dir ('{0:D2}-{1}-{2}.log' -f $k, $o.STEP, $o.DD)), $o.TEXT, $LAT); $k++
  }
  # console
  Write-Host ''
  foreach ($l in $script:LOG) { Write-Host $l }
  Write-Host ''
  Write-Host (' Saida do job (spool): spool{0}{1}_{2}' -f $SEP, $jn, $script:JOBID)
  Write-Host ''
  # limpa a area de trabalho do job
  Get-ChildItem -LiteralPath $Work -Filter ($script:JOBID + '_*') | Remove-Item -Force
}

# ======================================================================
#  VER (BROWSE) E LISTAR
# ======================================================================
function Browse([string]$dsn) {
  $dsn = $dsn.ToUpper()
  $res = ResolveDsn $dsn
  if ($res.ERROR) { Write-Host $res.ERROR; return }
  $name = $res.DSN
  if (GdgIsBase $dsn) { Write-Host " GDG BASE $dsn"; foreach ($g in (GdgGens $dsn)) { Write-Host ('   ' + (GdgName $dsn $g)) }; return }
  if (-not (DsExists $name)) { Write-Host " IKJ56228I DATA SET $name NOT IN CATALOG"; return }
  if ((DsIsPds $name) -and -not $res.MEMBER) {
    Write-Host " MEMBROS DE $name"; foreach ($f in (Get-ChildItem -LiteralPath (DsFile $name) -File)) { Write-Host ('   ' + $f.Name) }; return
  }
  $at = GetAttr $name
  if ($res.MEMBER) { $p = MemberFile $name $res.MEMBER; $recs = ReadRecords $p 80 $true } else { $recs = ReadRecords (DsFile $name) $at.LRECL $false }
  Write-Host (' BROWSE {0}   RECFM={1} LRECL={2}   {3} REGISTROS' -f $dsn, $at.RECFM, $at.LRECL, $recs.Count)
  $ruler = ''; for ($c = 1; $c -le [Math]::Min($at.LRECL, 120); $c++) { if ($c % 10 -eq 0) { $ruler += [string](($c / 10) % 10) } elseif ($c % 5 -eq 0) { $ruler += '+' } else { $ruler += '-' } }
  Write-Host (' ------ ' + $ruler)
  $i = 0
  foreach ($r in $recs) { $i++; Write-Host ('{0,6}  {1}' -f $i, $LAT.GetString($r)) }
}

# ======================================================================
#  PRINCIPAL
# ======================================================================
if (-not $Alvo) { Write-Host 'Uso: SUBMIT arquivo.jcl  |  SUBMIT DSN(MEMBRO)  |  VER DSN'; exit 8 }
if ($Ver) { Browse $Alvo; exit 0 }

$path = $null
if (Test-Path -LiteralPath $Alvo -PathType Leaf) { $path = (Resolve-Path -LiteralPath $Alvo).Path }
else {
  $r = ResolveDsn $Alvo.ToUpper()
  if ($r.MEMBER -and (DsIsPds $r.DSN)) { $path = MemberFile $r.DSN $r.MEMBER }
}
if (-not $path) { Write-Host " SIM003E JCL NAO ENCONTRADO: $Alvo"; exit 8 }
$lines = [IO.File]::ReadAllLines($path, $LAT)
$parsed = ParseJcl $lines
$job = $null
if ($parsed.ERR.Count -eq 0) { $job = BuildJob $parsed }
if ($job -eq $null) {
  $jn = 'JCLERR'
  foreach ($s in $parsed.STMTS) { if ($s.OP -eq 'JOB' -and $s.NAME) { $jn = $s.NAME; break } }
  if ($parsed.ERR.Count -eq 0) { $parsed.ERR.Add('       1 IEFC607I JOB HAS NO JOB STATEMENT') }
  $job = @{ NAME = $jn; STEPS = @() }
}
RunJob $job $parsed ($lines -join "`n")
