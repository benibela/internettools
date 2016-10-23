

//Here you can choose which regex library to use
{$if not defined(USE_SOROKINS_REGEX) and not defined(USE_SOROKINS_DREGEX) and not defined(USE_FLRE) and not defined(USE_FLRE_WITH_CACHE)}
//{$DEFINE USE_SOROKINS_REGEX}         //Sorokins' regex library included in FPC. It is quite reliable
//{$DEFINE USE_SOROKINS_DREGEX}        //my copy of Sorokins' library, for older FPC versions that did not include it
//{$DEFINE USE_FLRE}                   //Bero's "Fast Light Regular Expressions" library. It is required for proper UTF-8 support in regex
{$DEFINE USE_FLRE_WITH_CACHE}          //As above, but parse every regex only once
{$endif}



{$IFDEF USE_FLRE_WITH_CACHE}{$DEFINE USE_FLRE}{$ENDIF}





//internal flags
{$mode objfpc} {$H+}
{$modeswitch advancedrecords}


