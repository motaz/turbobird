// Common definitions for TurboBird
const
  {$IFDEF LINUX}
  Target = 'Linux';
  {$ENDIF}

  {$IFDEF WINDOWS}
  Target = 'Win';
  {$ENDIF}

  {$IFDEF BSD}
  {$IFDEF DARWIN}
  Target = 'Mac'; //Mac OSX
  {$ELSE}
  Target = 'BSD'; //FreeBSD, OpenBSD, NetBSD,...
  {$ENDIF}
  {$ENDIF}

  {$ifDEF CPU32}
  Arch = '32';
  {$ENDIF}

  {$ifDEF CPU64}
  Arch = '64';
  {$ENDIF}
