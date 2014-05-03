// Common definitions for TurboBird
const
  {$IFDEF LINUX}
   Target = 'Linux';
  {$ENDIF}

  {$IFDEF WINDOWS}
   Target = 'Win';
  {$ENDIF}

  {$IFDEF DARWIN}
   Target = 'Mac';
  {$ENDIF}

  {$IFDEF BSD}
   Target = 'BSD';
  {$ENDIF}

  {$ifDEF CPU32}
   Arch = '32';
  {$ENDIF}

  {$ifDEF CPU64}
   Arch = '64';
  {$ENDIF}
