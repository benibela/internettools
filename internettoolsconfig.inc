//Here you can choose which regex library to use
{$if not defined(USE_SOROKINS_REGEX) and not defined(USE_SOROKINS_DREGEX) and not defined(USE_FLRE) and not defined(USE_FLRE_WITH_CACHE)}
//{$DEFINE USE_SOROKINS_REGEX}         //Sorokins' regex library included in FPC. It is quite reliable
//{$DEFINE USE_FLRE}                   //Bero's "Fast Light Regular Expressions" library. It is required for proper UTF-8 support in regex
{$DEFINE USE_FLRE_WITH_CACHE}          //As above, but parse every regex only once
{$endif}




//Here you can choose which Unicode database to use
{$if not defined(USE_THEO_UNICODE) and not defined(USE_BBFLRE_UNICODE) and not defined(USE_BBFULL_UNICODE) }
//{$DEFINE USE_BBFLRE_UNICODE}    //FLRE's Unicode database together with normalization data required for XQuery. If FLRE is used anyways, this minimizes the file size
//{$DEFINE USE_BBFULL_UNICODE}  //My upgraded port of UTF8Proc. It is complete and self-contained, but quite large.
//{$DEFINE USE_THEO_UNICODE}    //Theo's port of UTF8Proc: Utf8 Tools. If you are already using it elsewhere, you might want to use it to avoid duplicated Unicode tables
{$endif}

{$DEFINE ALLOW_EXTERNAL_DOC_DOWNLOAD}



//internal checks
{$IFDEF USE_FLRE_WITH_CACHE}{$DEFINE USE_FLRE}{$ENDIF}
{$ifdef USE_SOROKINS_DREGEX}{$define USE_SOROKINS_REGEX}{$endif}

{$if not defined(USE_SOROKINS_REGEX) and not defined(USE_FLRE) and not defined(USE_FLRE_WITH_CACHE)}
{$error you need to enable one regex library define in internettoolsconfig.inc}
{$endif}

{$if not defined(USE_BBFLRE_UNICODE) and not defined(USE_BBFULL_UNICODE) and not defined(USE_THEO_UNICODE) }
{$ifdef USE_FLRE}{$define USE_BBFLRE_UNICODE}{$else}{$define USE_BBFULL_UNICODE}{$endif}
{$endif}






//internal flags
{$mode objfpc} {$H+}
{$modeswitch advancedrecords}


