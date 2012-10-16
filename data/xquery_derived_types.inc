
//nice picture http://www.w3.org/TR/xpath-datamodel/type-hierarchy.png
//http://www.w3.org/TR/xpath-functions/#casting
// http://msdn.microsoft.com/en-us/library/ms191231.aspx

{$ifdef PXP_DERIVED_TYPES_INTERFACE}
const MININT = -MAXINT - 1;
type TXQ_Integer = int65;
type TXQ_Int65 = int65;
type TXQ_Decimal = decimal;
type TXQ_string = string;
type TXQ_datetime = string;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
type TClassArray = array of TXQValueClass;

//todo: fix this (see http://www.w3.org/TR/REC-xml/#NT-Letter). Problem, our regex engine is not unicode compatible
const RegExpr_XMLLetter = '[A-Za-z]';
const RegExpr_XMLNCNameChar = '[^:'#1'-'#32']';
const RegExpr_XMLNameChar = '(('+RegExpr_XMLNCNameChar+')|:)';
const RegExpr_XMLNameRaw = '('+RegExpr_XMLLetter+'|[_:])('+RegExpr_XMLNameChar+')*';
const RegExpr_XMLName = '^'+RegExpr_XMLNameRaw+'$';
const RegExpr_XMLNCNameRaw = '('+RegExpr_XMLLetter+'|_)('+RegExpr_XMLNCNameChar+')*'; //name without :
const RegExpr_XMLNCName = '^'+RegExpr_XMLNCNameRaw+'$'; //name without :
const RegExpr__language = '^[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*$';
//URI as defined in RFC 2396, 2732, 2373 (see commit log)
const RegExpr_anyURIRaw = '((((([a-zA-Z][a-zA-Z0-9+-.]*:)?((//(((([a-zA-Z0-9-_.!~*''();:&=+$,]|%[a-fA-F0-9]{2})*@)?((([a-zA-Z0-9]([-a-zA-Z0-9]*[a-zA-Z0-9])?.)*[a-zA-Z]([-a-zA-Z0-9]*[a-zA-Z0-9])?.?)|([0-9]+(.[0-9]+){3})|\[(([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*)?|([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*)?::([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*)?)(:[0-9]{1,3}(.[0-9]{1,3}){3})?\])(:[0-9]*)?)?|([a-zA-Z0-9-_.!~*''()$,;:@&=+]|%[a-fA-F0-9]{2})+)(/([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*(;([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*)*(/([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*(;([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*)*)*)?)|(/([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*(;([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*)*(/([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*(;([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*)*)*)))|(([a-zA-Z0-9-_.!~*''();@&=+$,]|%[a-fA-F0-9]{2})+(/([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*(;([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*)*(/([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*(;([a-zA-Z0-9-_.!~*''():@&=+$,]|%[a-fA-F0-9]{2})*)*)*)?))([?]([;/?:@&=+$,\][a-zA-Z0-9-_.!~*''()]|%[a-fA-F0-9]{2})*)?)|([a-zA-Z][a-zA-Z0-9+-.]*:([a-zA-Z0-9-_.!~*''();?:@&=+$,]|%[a-fA-F0-9]{2})(([;/?:@&=+$,\][a-zA-Z0-9-_.!~*''()]|%[a-fA-F0-9]{2}))*))?(#(([;/?:@&=+$,\][a-zA-Z0-9-_.!~*''()]|%[a-fA-F0-9]{2})*))?';
const RegExpr_QName = '^('+RegExpr_anyURIRaw+'['+#2+'])?'+RegExpr_XMLNameRaw+'$';
{$endif}

//Template parameter to create a new type:
//__NAME__: PXP type name (add TXQValue_ to get FPC name)
//T__Parent__: FPC parent type name
//__T_Base_Element__: primary type of new type
//__RANGE_MIN__: Minimal value, only used for integer, FULLRANGE allows all values
//__RANGE_MAX__: For integer: Maximal value, FULLRANGE allows all values || For string: regex validating the string
//__HIERARCHY_BLOCK__: If set to BLOCKED, the new type is not an "instance of" the old type
//__CASTABLE_FROM__: |-separated list of all types, which may cast to the new type. (If empty, default behaviour). * before a type allows the casting unconditionally







{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type long. (automatic generated)
type TXQValue_long = class (TXQValueInt65)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_long.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_long.classTypeName: string;
begin
  result := 'long';
end;
class function TXQValue_long.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_long.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_long.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  
  
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_long);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}









{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type int. (automatic generated)
type TXQValue_int = class (TXQValue_Long)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_int.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  
  if tv < -2147483648 then begin
    tv := (tv + $7000000000000000) and  (2147483647  );
  end;
  
  if tv > 2147483647 then begin
    tv := tv and  UInt64(2147483647  );
  end;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_int.classTypeName: string;
begin
  result := 'int';
end;
class function TXQValue_int.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_int.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_int.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  if v < -2147483648 then exit(false); 
  if v > 2147483647 then exit(false); 
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_int);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('type-of(xs:int(-2147483648))', 'int', '');



t('type-of(xs:int(2147483647))', 'int', '');





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type short. (automatic generated)
type TXQValue_short = class (TXQValue_Int)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_short.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  
  if tv < -32768 then begin
    tv := (tv + $7000000000000000) and  (32767  );
  end;
  
  if tv > 32767 then begin
    tv := tv and  UInt64(32767  );
  end;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_short.classTypeName: string;
begin
  result := 'short';
end;
class function TXQValue_short.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_short.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_short.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  if v < -32768 then exit(false); 
  if v > 32767 then exit(false); 
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_short);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('type-of(xs:short(-32768))', 'short', '');



t('type-of(xs:short(32767))', 'short', '');





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type byte. (automatic generated)
type TXQValue_byte = class (TXQValue_Short)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_byte.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  
  if tv < -128 then begin
    tv := (tv + $7000000000000000) and  (127  );
  end;
  
  if tv > 127 then begin
    tv := tv and  UInt64(127  );
  end;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_byte.classTypeName: string;
begin
  result := 'byte';
end;
class function TXQValue_byte.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_byte.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_byte.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  if v < -128 then exit(false); 
  if v > 127 then exit(false); 
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_byte);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('type-of(xs:byte(-128))', 'byte', '');



t('type-of(xs:byte(127))', 'byte', '');





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type nonPositiveInteger. (automatic generated)
type TXQValue_nonPositiveInteger = class (TXQValueInt65)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_nonPositiveInteger.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  
  if tv > 0 then begin
    tv := tv and  UInt64(0  );
  end;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_nonPositiveInteger.classTypeName: string;
begin
  result := 'nonPositiveInteger';
end;
class function TXQValue_nonPositiveInteger.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_nonPositiveInteger.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_nonPositiveInteger.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  
  if v > 0 then exit(false); 
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_nonPositiveInteger);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





t('type-of(xs:nonPositiveInteger(0))', 'nonPositiveInteger', '');





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type negativeInteger. (automatic generated)
type TXQValue_negativeInteger = class (TXQValue_NonPositiveInteger)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_negativeInteger.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  
  if tv > -1 then begin
    tv := tv and  UInt64(-1  );
  end;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_negativeInteger.classTypeName: string;
begin
  result := 'negativeInteger';
end;
class function TXQValue_negativeInteger.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_negativeInteger.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_negativeInteger.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  
  if v > -1 then exit(false); 
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_negativeInteger);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





t('type-of(xs:negativeInteger(-1))', 'negativeInteger', '');





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type nonNegativeInteger. (automatic generated)
type TXQValue_nonNegativeInteger = class (TXQValueInt65)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_nonNegativeInteger.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_nonNegativeInteger.classTypeName: string;
begin
  result := 'nonNegativeInteger';
end;
class function TXQValue_nonNegativeInteger.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_nonNegativeInteger.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_nonNegativeInteger.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  if v < 0 then exit(false); 
  
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_nonNegativeInteger);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('type-of(xs:nonNegativeInteger(0))', 'nonNegativeInteger', '');







{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type positiveInteger. (automatic generated)
type TXQValue_positiveInteger = class (TXQValue_NonNegativeInteger)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_positiveInteger.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_positiveInteger.classTypeName: string;
begin
  result := 'positiveInteger';
end;
class function TXQValue_positiveInteger.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_positiveInteger.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_positiveInteger.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  if v < 1 then exit(false); 
  
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_positiveInteger);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('type-of(xs:positiveInteger(1))', 'positiveInteger', '');







{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type unsignedLong. (automatic generated)
type TXQValue_unsignedLong = class (TXQValue_NonNegativeInteger)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_unsignedLong.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_unsignedLong.classTypeName: string;
begin
  result := 'unsignedLong';
end;
class function TXQValue_unsignedLong.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_unsignedLong.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_unsignedLong.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  if v < 0 then exit(false); 
  
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_unsignedLong);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('type-of(xs:unsignedLong(0))', 'unsignedLong', '');







{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type unsignedInt. (automatic generated)
type TXQValue_unsignedInt = class (TXQValue_unsignedLong)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_unsignedInt.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  
  if tv < 0 then begin
    tv := (tv + $7000000000000000) and  (4294967295  );
  end;
  
  if tv > 4294967295 then begin
    tv := tv and  UInt64(4294967295  );
  end;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_unsignedInt.classTypeName: string;
begin
  result := 'unsignedInt';
end;
class function TXQValue_unsignedInt.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_unsignedInt.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_unsignedInt.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  if v < 0 then exit(false); 
  if v > 4294967295 then exit(false); 
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_unsignedInt);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('type-of(xs:unsignedInt(0))', 'unsignedInt', '');



t('type-of(xs:unsignedInt(4294967295))', 'unsignedInt', '');





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type unsignedShort. (automatic generated)
type TXQValue_unsignedShort = class (TXQValue_unsignedInt)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_unsignedShort.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  
  if tv < 0 then begin
    tv := (tv + $7000000000000000) and  (65536  );
  end;
  
  if tv > 65536 then begin
    tv := tv and  UInt64(65536  );
  end;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_unsignedShort.classTypeName: string;
begin
  result := 'unsignedShort';
end;
class function TXQValue_unsignedShort.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_unsignedShort.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_unsignedShort.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  if v < 0 then exit(false); 
  if v > 65536 then exit(false); 
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_unsignedShort);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('type-of(xs:unsignedShort(0))', 'unsignedShort', '');



t('type-of(xs:unsignedShort(65536))', 'unsignedShort', '');





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type unsignedByte. (automatic generated)
type TXQValue_unsignedByte = class (TXQValue_unsignedShort)
  constructor create(const v: TXQ_int65); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromint65(const v: TXQ_int65): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_unsignedByte.create(const v: TXQ_int65);
var tv: TXQ_int65;
begin
  
  tv := v;
  
  
  if tv < 0 then begin
    tv := (tv + $7000000000000000) and  (255  );
  end;
  
  if tv > 255 then begin
    tv := tv and  UInt64(255  );
  end;
  
  value := tv;
  

  

  

  
end;
class function TXQValue_unsignedByte.classTypeName: string;
begin
  result := 'unsignedByte';
end;
class function TXQValue_unsignedByte.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_unsignedByte.create(v.toint65));
  result := inherited; ignore(v);
end;




class function TXQValue_unsignedByte.canCreateFromint65(const v: TXQ_int65): boolean;
begin
  result := true; ignore(v);
  
  if v < 0 then exit(false); 
  if v > 255 then exit(false); 
  
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_unsignedByte);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('type-of(xs:unsignedByte(0))', 'unsignedByte', '');



t('type-of(xs:unsignedByte(255))', 'unsignedByte', '');





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type float. (automatic generated)
type TXQValue_float = class (TXQValueDecimal)
  constructor create(const v: TXQ_decimal); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromdecimal(const v: TXQ_decimal): boolean; override;
  
  
  class function truncateRange(const v: TXQ_decimal): TXQ_decimal;override;
  function toString: string; override;
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_float.create(const v: TXQ_decimal);

begin
  

  
  value := truncateRange(v);
  

  

  
end;
class function TXQValue_float.classTypeName: string;
begin
  result := 'float';
end;
class function TXQValue_float.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_float.create(v.todecimal));
  result := inherited; ignore(v);
end;

class function TXQValue_float.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_float then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_float.canCreateFromdecimal(const v: TXQ_decimal): boolean;
begin
  result := true; ignore(v);
  
  
  

end;



class function TXQValue_float.truncateRange(const v: TXQ_decimal): TXQ_decimal;
begin
  result := single  (v);
end;

function TXQValue_float.toString: string;
begin
  if isnan(value) then result:='NaN'
  else if isPosInf(value) then result:='INF'
  else if isNegInf(value) then result:='-INF'
  else result := myDecimalToStr(single  (value)); //this is needed to call the correct overloaded toStr function to print a correctly rounded value
end;





{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_float);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}









{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type double. (automatic generated)
type TXQValue_double = class (TXQValueDecimal)
  constructor create(const v: TXQ_decimal); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromdecimal(const v: TXQ_decimal): boolean; override;
  
  
  class function truncateRange(const v: TXQ_decimal): TXQ_decimal;override;
  function toString: string; override;
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_double.create(const v: TXQ_decimal);

begin
  

  
  value := truncateRange(v);
  

  

  
end;
class function TXQValue_double.classTypeName: string;
begin
  result := 'double';
end;
class function TXQValue_double.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_double.create(v.todecimal));
  result := inherited; ignore(v);
end;

class function TXQValue_double.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_double then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_double.canCreateFromdecimal(const v: TXQ_decimal): boolean;
begin
  result := true; ignore(v);
  
  
  

end;



class function TXQValue_double.truncateRange(const v: TXQ_decimal): TXQ_decimal;
begin
  result :=  double (v);
end;

function TXQValue_double.toString: string;
begin
  if isnan(value) then result:='NaN'
  else if isPosInf(value) then result:='INF'
  else if isNegInf(value) then result:='-INF'
  else result := myDecimalToStr( double (value)); //this is needed to call the correct overloaded toStr function to print a correctly rounded value
end;





{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_double);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}









{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type untypedAtomic. (automatic generated)
type TXQValue_untypedAtomic = class (TXQValueString)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExpruntypedAtomic: TRegexpr = nil;

constructor TXQValue_untypedAtomic.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type untypedAtomic');
  

  
end;
class function TXQValue_untypedAtomic.classTypeName: string;
begin
  result := 'untypedAtomic';
end;
class function TXQValue_untypedAtomic.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_untypedAtomic.create(v.tostring));
  result := inherited; ignore(v);
end;

class function TXQValue_untypedAtomic.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_untypedAtomic then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_untypedAtomic.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExpruntypedAtomic = nil) then RegExpruntypedAtomic := tregexpr.create('.*');
  result := RegExpruntypedAtomic.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_untypedAtomic);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExpruntypedAtomic.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:untypedAtomic(''abc'')', 'abc', '');
t('type-of(xs:untypedAtomic(''abc''))', 'untypedAtomic', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type normalizedString. (automatic generated)
type TXQValue_normalizedString = class (TXQValueString)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprnormalizedString: TRegexpr = nil;

constructor TXQValue_normalizedString.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type normalizedString');
  

  
end;
class function TXQValue_normalizedString.classTypeName: string;
begin
  result := 'normalizedString';
end;
class function TXQValue_normalizedString.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_normalizedString.create(v.tostring));
  result := inherited; ignore(v);
end;




class function TXQValue_normalizedString.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprnormalizedString = nil) then RegExprnormalizedString := tregexpr.create('^[^'#9#13#10']*$');
  result := RegExprnormalizedString.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_normalizedString);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprnormalizedString.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:normalizedString(''abc'')', 'abc', '');
t('type-of(xs:normalizedString(''abc''))', 'normalizedString', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type token. (automatic generated)
type TXQValue_token = class (TXQValue_normalizedString)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprtoken: TRegexpr = nil;

constructor TXQValue_token.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type token');
  

  
end;
class function TXQValue_token.classTypeName: string;
begin
  result := 'token';
end;
class function TXQValue_token.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_token.create(v.tostring));
  result := inherited; ignore(v);
end;




class function TXQValue_token.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprtoken = nil) then RegExprtoken := tregexpr.create('^(([^'#9#13#10' ]+|[^'#9#13#10' ] )*[^'#9#13#10' ])?$');
  result := RegExprtoken.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_token);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprtoken.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:token(''abc'')', 'abc', '');
t('type-of(xs:token(''abc''))', 'token', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type language. (automatic generated)
type TXQValue_language = class (TXQValue_token)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprlanguage: TRegexpr = nil;

constructor TXQValue_language.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type language');
  

  
end;
class function TXQValue_language.classTypeName: string;
begin
  result := 'language';
end;
class function TXQValue_language.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_language.create(v.tostring));
  result := inherited; ignore(v);
end;




class function TXQValue_language.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprlanguage = nil) then RegExprlanguage := tregexpr.create(RegExpr__language);
  result := RegExprlanguage.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_language);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprlanguage.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:language(''abc'')', 'abc', '');
t('type-of(xs:language(''abc''))', 'language', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type NMTOKEN. (automatic generated)
type TXQValue_NMTOKEN = class (TXQValue_token)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprNMTOKEN: TRegexpr = nil;

constructor TXQValue_NMTOKEN.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type NMTOKEN');
  

  
end;
class function TXQValue_NMTOKEN.classTypeName: string;
begin
  result := 'NMTOKEN';
end;
class function TXQValue_NMTOKEN.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_NMTOKEN.create(v.tostring));
  result := inherited; ignore(v);
end;




class function TXQValue_NMTOKEN.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprNMTOKEN = nil) then RegExprNMTOKEN := tregexpr.create('^('+RegExpr_XMLNameChar+')+$');
  result := RegExprNMTOKEN.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_NMTOKEN);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprNMTOKEN.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:NMTOKEN(''abc'')', 'abc', '');
t('type-of(xs:NMTOKEN(''abc''))', 'NMTOKEN', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type Name. (automatic generated)
type TXQValue_Name = class (TXQValue_token)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprName: TRegexpr = nil;

constructor TXQValue_Name.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type Name');
  

  
end;
class function TXQValue_Name.classTypeName: string;
begin
  result := 'Name';
end;
class function TXQValue_Name.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_Name.create(v.tostring));
  result := inherited; ignore(v);
end;




class function TXQValue_Name.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprName = nil) then RegExprName := tregexpr.create(RegExpr_XMLName);
  result := RegExprName.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_Name);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprName.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:Name(''abc'')', 'abc', '');
t('type-of(xs:Name(''abc''))', 'Name', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type NCName. (automatic generated)
type TXQValue_NCName = class (TXQValue_Name)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprNCName: TRegexpr = nil;

constructor TXQValue_NCName.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type NCName');
  

  
end;
class function TXQValue_NCName.classTypeName: string;
begin
  result := 'NCName';
end;
class function TXQValue_NCName.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_NCName.create(v.tostring));
  result := inherited; ignore(v);
end;




class function TXQValue_NCName.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprNCName = nil) then RegExprNCName := tregexpr.create(RegExpr_XMLNCName);
  result := RegExprNCName.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_NCName);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprNCName.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:NCName(''abc'')', 'abc', '');
t('type-of(xs:NCName(''abc''))', 'NCName', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type ID. (automatic generated)
type TXQValue_ID = class (TXQValue_NCNAME)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprID: TRegexpr = nil;

constructor TXQValue_ID.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type ID');
  

  
end;
class function TXQValue_ID.classTypeName: string;
begin
  result := 'ID';
end;
class function TXQValue_ID.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_ID.create(v.tostring));
  result := inherited; ignore(v);
end;




class function TXQValue_ID.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprID = nil) then RegExprID := tregexpr.create(RegExpr_XMLNCName);
  result := RegExprID.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_ID);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprID.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:ID(''abc'')', 'abc', '');
t('type-of(xs:ID(''abc''))', 'ID', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type IDREF. (automatic generated)
type TXQValue_IDREF = class (TXQValue_NCNAME)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprIDREF: TRegexpr = nil;

constructor TXQValue_IDREF.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type IDREF');
  

  
end;
class function TXQValue_IDREF.classTypeName: string;
begin
  result := 'IDREF';
end;
class function TXQValue_IDREF.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_IDREF.create(v.tostring));
  result := inherited; ignore(v);
end;




class function TXQValue_IDREF.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprIDREF = nil) then RegExprIDREF := tregexpr.create(RegExpr_XMLNCName);
  result := RegExprIDREF.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_IDREF);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprIDREF.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:IDREF(''abc'')', 'abc', '');
t('type-of(xs:IDREF(''abc''))', 'IDREF', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type ENTITY. (automatic generated)
type TXQValue_ENTITY = class (TXQValue_NCNAME)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprENTITY: TRegexpr = nil;

constructor TXQValue_ENTITY.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type ENTITY');
  

  
end;
class function TXQValue_ENTITY.classTypeName: string;
begin
  result := 'ENTITY';
end;
class function TXQValue_ENTITY.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_ENTITY.create(v.tostring));
  result := inherited; ignore(v);
end;




class function TXQValue_ENTITY.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprENTITY = nil) then RegExprENTITY := tregexpr.create(RegExpr_XMLNCName);
  result := RegExprENTITY.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_ENTITY);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprENTITY.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:ENTITY(''abc'')', 'abc', '');
t('type-of(xs:ENTITY(''abc''))', 'ENTITY', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type anyURI. (automatic generated)
type TXQValue_anyURI = class (TXQValueString)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  class function castableFromInternal(const v: IXQValue): boolean; override;
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExpranyURI: TRegexpr = nil;

constructor TXQValue_anyURI.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type anyURI');
  

  
end;
class function TXQValue_anyURI.classTypeName: string;
begin
  result := 'anyURI';
end;
class function TXQValue_anyURI.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_anyURI.create(v.tostring));
  result := inherited; ignore(v);
end;

class function TXQValue_anyURI.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_anyURI then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;



const CastableFromanyURI: TClassArray = nil;
const CastableFromanyURISplit: TStringArray = nil;
class function TXQValue_anyURI.castableFromInternal(const v: IXQValue): boolean;
var i,t: integer; temp: string;
begin
  if v.instanceOfInternal(self) then exit(true);
  if CastableFromanyURI = nil then begin
    CastableFromanyURISplit := strSplit('string|untypedAtomic', '|');
    setlength(CastableFromanyURI, length(CastableFromanyURISplit));
    for i:=0 to high(CastableFromanyURISplit) do begin
      temp := CastableFromanyURISplit[i];
      if temp[1] in ['*'] then delete(temp,1,1);
      t := types.indexOf(temp);
      if t < 0 then raise exception.create('Could not find type: '+temp);
      CastableFromanyURI[i] := TXQValueClass(types.objects[t]);
    end;
  end;
  result := false;
  for i:=0 to high(CastableFromanyURI) do
    if v.instanceOfInternal(CastableFromanyURI[i]) then begin
      result := true;
      if CastableFromanyURISplit[i][1] = '*' then exit;
      break;
    end;
  if result then result := inherited;
end;


class function TXQValue_anyURI.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExpranyURI = nil) then RegExpranyURI := tregexpr.create('^'+RegExpr_anyURIRaw+'$');
  result := RegExpranyURI.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_anyURI);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExpranyURI.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:anyURI(''abc'')', 'abc', '');
t('type-of(xs:anyURI(''abc''))', 'anyURI', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type QName. (automatic generated)
type TXQValue_QName = class (TXQValue_anyURI)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  class function castableFromInternal(const v: IXQValue): boolean; override;
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprQName: TRegexpr = nil;

constructor TXQValue_QName.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type QName');
  

  
end;
class function TXQValue_QName.classTypeName: string;
begin
  result := 'QName';
end;
class function TXQValue_QName.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_QName.create(v.tostring));
  result := inherited; ignore(v);
end;

class function TXQValue_QName.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_QName then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;



const CastableFromQName: TClassArray = nil;
const CastableFromQNameSplit: TStringArray = nil;
class function TXQValue_QName.castableFromInternal(const v: IXQValue): boolean;
var i,t: integer; temp: string;
begin
  if v.instanceOfInternal(self) then exit(true);
  if CastableFromQName = nil then begin
    CastableFromQNameSplit := strSplit('*QName|string', '|');
    setlength(CastableFromQName, length(CastableFromQNameSplit));
    for i:=0 to high(CastableFromQNameSplit) do begin
      temp := CastableFromQNameSplit[i];
      if temp[1] in ['*'] then delete(temp,1,1);
      t := types.indexOf(temp);
      if t < 0 then raise exception.create('Could not find type: '+temp);
      CastableFromQName[i] := TXQValueClass(types.objects[t]);
    end;
  end;
  result := false;
  for i:=0 to high(CastableFromQName) do
    if v.instanceOfInternal(CastableFromQName[i]) then begin
      result := true;
      if CastableFromQNameSplit[i][1] = '*' then exit;
      break;
    end;
  if result then result := inherited;
end;


class function TXQValue_QName.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprQName = nil) then RegExprQName := tregexpr.create(RegExpr_QName);
  result := RegExprQName.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_QName);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprQName.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:QName(''abc'')', 'abc', '');
t('type-of(xs:QName(''abc''))', 'QName', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type NOTATION. (automatic generated)
type TXQValue_NOTATION = class (TXQValue_QName)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  class function castableFromInternal(const v: IXQValue): boolean; override;
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprNOTATION: TRegexpr = nil;

constructor TXQValue_NOTATION.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type NOTATION');
  

  
end;
class function TXQValue_NOTATION.classTypeName: string;
begin
  result := 'NOTATION';
end;
class function TXQValue_NOTATION.createFromValue(const v: IXQValue): IXQValue;
begin
  
  exit(TXQValue_NOTATION.create(v.tostring));
  result := inherited; ignore(v);
end;

class function TXQValue_NOTATION.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_NOTATION then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;



const CastableFromNOTATION: TClassArray = nil;
const CastableFromNOTATIONSplit: TStringArray = nil;
class function TXQValue_NOTATION.castableFromInternal(const v: IXQValue): boolean;
var i,t: integer; temp: string;
begin
  if v.instanceOfInternal(self) then exit(true);
  if CastableFromNOTATION = nil then begin
    CastableFromNOTATIONSplit := strSplit('NOTATION|string', '|');
    setlength(CastableFromNOTATION, length(CastableFromNOTATIONSplit));
    for i:=0 to high(CastableFromNOTATIONSplit) do begin
      temp := CastableFromNOTATIONSplit[i];
      if temp[1] in ['*'] then delete(temp,1,1);
      t := types.indexOf(temp);
      if t < 0 then raise exception.create('Could not find type: '+temp);
      CastableFromNOTATION[i] := TXQValueClass(types.objects[t]);
    end;
  end;
  result := false;
  for i:=0 to high(CastableFromNOTATION) do
    if v.instanceOfInternal(CastableFromNOTATION[i]) then begin
      result := true;
      if CastableFromNOTATIONSplit[i][1] = '*' then exit;
      break;
    end;
  if result then result := inherited;
end;


class function TXQValue_NOTATION.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprNOTATION = nil) then RegExprNOTATION := tregexpr.create(RegExpr_QName);
  result := RegExprNOTATION.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_NOTATION);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprNOTATION.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



t('xs:NOTATION(''abc'')', 'abc', '');
t('type-of(xs:NOTATION(''abc''))', 'NOTATION', '');

{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type hexBinary. (automatic generated)
type TXQValue_hexBinary = class (TXQValue_Binary)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  class function castableFromInternal(const v: IXQValue): boolean; override;
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprhexBinary: TRegexpr = nil;

constructor TXQValue_hexBinary.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
   str := uppercase(str); 
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type hexBinary');
  

  
end;
class function TXQValue_hexBinary.classTypeName: string;
begin
  result := 'hexBinary';
end;
class function TXQValue_hexBinary.createFromValue(const v: IXQValue): IXQValue;
begin
  exit(inherited);
  exit(TXQValue_hexBinary.create(v.tostring));
  result := inherited; ignore(v);
end;



const CastableFromhexBinary: TClassArray = nil;
const CastableFromhexBinarySplit: TStringArray = nil;
class function TXQValue_hexBinary.castableFromInternal(const v: IXQValue): boolean;
var i,t: integer; temp: string;
begin
  if v.instanceOfInternal(self) then exit(true);
  if CastableFromhexBinary = nil then begin
    CastableFromhexBinarySplit := strSplit('string|untypedAtomic|*base64Binary', '|');
    setlength(CastableFromhexBinary, length(CastableFromhexBinarySplit));
    for i:=0 to high(CastableFromhexBinarySplit) do begin
      temp := CastableFromhexBinarySplit[i];
      if temp[1] in ['*'] then delete(temp,1,1);
      t := types.indexOf(temp);
      if t < 0 then raise exception.create('Could not find type: '+temp);
      CastableFromhexBinary[i] := TXQValueClass(types.objects[t]);
    end;
  end;
  result := false;
  for i:=0 to high(CastableFromhexBinary) do
    if v.instanceOfInternal(CastableFromhexBinary[i]) then begin
      result := true;
      if CastableFromhexBinarySplit[i][1] = '*' then exit;
      break;
    end;
  if result then result := inherited;
end;


class function TXQValue_hexBinary.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprhexBinary = nil) then RegExprhexBinary := tregexpr.create('^([0-9A-Fa-f][0-9A-Fa-f])*$');
  result := RegExprhexBinary.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_hexBinary);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprhexBinary.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type base64Binary. (automatic generated)
type TXQValue_base64Binary = class (TXQValue_Binary)
  constructor create(const v: TXQ_string); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  class function castableFromInternal(const v: IXQValue): boolean; override;
  class function canCreateFromstring(const v: TXQ_string): boolean; override;
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}
 const RegExprbase64Binary: TRegexpr = nil;

constructor TXQValue_base64Binary.create(const v: TXQ_string);

begin
  

  

  
  str := trim(v); //trim all derived types
  
  if not canCreateFromstring(str) then raise exception.create('Invalid value: '+v+' for type base64Binary');
  

  
end;
class function TXQValue_base64Binary.classTypeName: string;
begin
  result := 'base64Binary';
end;
class function TXQValue_base64Binary.createFromValue(const v: IXQValue): IXQValue;
begin
  exit(inherited);
  exit(TXQValue_base64Binary.create(v.tostring));
  result := inherited; ignore(v);
end;



const CastableFrombase64Binary: TClassArray = nil;
const CastableFrombase64BinarySplit: TStringArray = nil;
class function TXQValue_base64Binary.castableFromInternal(const v: IXQValue): boolean;
var i,t: integer; temp: string;
begin
  if v.instanceOfInternal(self) then exit(true);
  if CastableFrombase64Binary = nil then begin
    CastableFrombase64BinarySplit := strSplit('string|untypedAtomic|*hexBinary', '|');
    setlength(CastableFrombase64Binary, length(CastableFrombase64BinarySplit));
    for i:=0 to high(CastableFrombase64BinarySplit) do begin
      temp := CastableFrombase64BinarySplit[i];
      if temp[1] in ['*'] then delete(temp,1,1);
      t := types.indexOf(temp);
      if t < 0 then raise exception.create('Could not find type: '+temp);
      CastableFrombase64Binary[i] := TXQValueClass(types.objects[t]);
    end;
  end;
  result := false;
  for i:=0 to high(CastableFrombase64Binary) do
    if v.instanceOfInternal(CastableFrombase64Binary[i]) then begin
      result := true;
      if CastableFrombase64BinarySplit[i][1] = '*' then exit;
      break;
    end;
  if result then result := inherited;
end;


class function TXQValue_base64Binary.canCreateFromstring(const v: TXQ_string): boolean;
begin
  result := true; ignore(v);
  
  
  if (RegExprbase64Binary = nil) then RegExprbase64Binary := tregexpr.create('^([A-Za-z0-9+/] *)*(|[AEIMQUYcgkosw048] *=|[AQgw] *= *=)$');
  result := RegExprbase64Binary.exec(v);
  
  

end;







{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_base64Binary);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}
 RegExprbase64Binary.free; 
{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}



{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type time. (automatic generated)
type TXQValue_time = class (TXQValueDateTime)
  constructor create(const v: TXQ_datetime); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromdatetime(const v: TXQ_datetime): boolean; override;
  
  class function dateFormat(): string; override;
  procedure truncateRange();override; 
  
  
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_time.create(const v: TXQ_datetime);

begin
  

  

  

  inherited; truncateRange();  
end;
class function TXQValue_time.classTypeName: string;
begin
  result := 'time';
end;
class function TXQValue_time.createFromValue(const v: IXQValue): IXQValue;
begin
  
  
  result := inherited; ignore(v);
end;

class function TXQValue_time.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_time then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_time.canCreateFromdatetime(const v: TXQ_datetime): boolean;
begin
  result := true; ignore(v);
  
  
  
  result:=inherited;
  

end;

class function TXQValue_time.dateFormat(): string;
begin
  result := 'hh:nn:ss[.z+][Z]$';
end;
 procedure TXQValue_time.truncateRange; begin value.year := 1989; value.month := 12; value.day := 30; end; 











{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_time);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type date. (automatic generated)
type TXQValue_date = class (TXQValueDateTime)
  constructor create(const v: TXQ_datetime); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromdatetime(const v: TXQ_datetime): boolean; override;
  
  class function dateFormat(): string; override;
  
  procedure truncateRange();override; 
  
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_date.create(const v: TXQ_datetime);

begin
  

  

  

  inherited; truncateRange();  
end;
class function TXQValue_date.classTypeName: string;
begin
  result := 'date';
end;
class function TXQValue_date.createFromValue(const v: IXQValue): IXQValue;
begin
  
  
  result := inherited; ignore(v);
end;

class function TXQValue_date.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_date then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_date.canCreateFromdatetime(const v: TXQ_datetime): boolean;
begin
  result := true; ignore(v);
  
  
  
  result:=inherited;
  

end;

class function TXQValue_date.dateFormat(): string;
begin
  result := 'yyyy-mm-dd[Z]$';
end;


 procedure TXQValue_date.truncateRange; begin value.hour := 0; value.min := 0; value.sec := 0; end; 









{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_date);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type gYear. (automatic generated)
type TXQValue_gYear = class (TXQValue_datepart)
  constructor create(const v: TXQ_datetime); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromdatetime(const v: TXQ_datetime): boolean; override;
  
  class function dateFormat(): string; override;
  
  
  
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_gYear.create(const v: TXQ_datetime);

begin
  

  

  

  inherited; truncateRange();  
end;
class function TXQValue_gYear.classTypeName: string;
begin
  result := 'gYear';
end;
class function TXQValue_gYear.createFromValue(const v: IXQValue): IXQValue;
begin
  
  
  result := inherited; ignore(v);
end;

class function TXQValue_gYear.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_gYear then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_gYear.canCreateFromdatetime(const v: TXQ_datetime): boolean;
begin
  result := true; ignore(v);
  
  
  
  result:=inherited;
  

end;

class function TXQValue_gYear.dateFormat(): string;
begin
  result := 'yyyy[Z]$';
end;












{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_gYear);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type gYearMonth. (automatic generated)
type TXQValue_gYearMonth = class (TXQValue_datepart)
  constructor create(const v: TXQ_datetime); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromdatetime(const v: TXQ_datetime): boolean; override;
  
  class function dateFormat(): string; override;
  
  
  
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_gYearMonth.create(const v: TXQ_datetime);

begin
  

  

  

  inherited; truncateRange();  
end;
class function TXQValue_gYearMonth.classTypeName: string;
begin
  result := 'gYearMonth';
end;
class function TXQValue_gYearMonth.createFromValue(const v: IXQValue): IXQValue;
begin
  
  
  result := inherited; ignore(v);
end;

class function TXQValue_gYearMonth.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_gYearMonth then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_gYearMonth.canCreateFromdatetime(const v: TXQ_datetime): boolean;
begin
  result := true; ignore(v);
  
  
  
  result:=inherited;
  

end;

class function TXQValue_gYearMonth.dateFormat(): string;
begin
  result := 'yyyy-mm[Z]$';
end;












{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_gYearMonth);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type gMonthDay. (automatic generated)
type TXQValue_gMonthDay = class (TXQValue_datepart)
  constructor create(const v: TXQ_datetime); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromdatetime(const v: TXQ_datetime): boolean; override;
  
  class function dateFormat(): string; override;
  
  
  
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_gMonthDay.create(const v: TXQ_datetime);

begin
  

  

  

  inherited; truncateRange();  
end;
class function TXQValue_gMonthDay.classTypeName: string;
begin
  result := 'gMonthDay';
end;
class function TXQValue_gMonthDay.createFromValue(const v: IXQValue): IXQValue;
begin
  
  
  result := inherited; ignore(v);
end;

class function TXQValue_gMonthDay.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_gMonthDay then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_gMonthDay.canCreateFromdatetime(const v: TXQ_datetime): boolean;
begin
  result := true; ignore(v);
  
  
  
  result:=inherited;
  

end;

class function TXQValue_gMonthDay.dateFormat(): string;
begin
  result := '--mm-dd[Z]$';
end;












{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_gMonthDay);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type gMonth. (automatic generated)
type TXQValue_gMonth = class (TXQValue_datepart)
  constructor create(const v: TXQ_datetime); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromdatetime(const v: TXQ_datetime): boolean; override;
  
  class function dateFormat(): string; override;
  
  
  
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_gMonth.create(const v: TXQ_datetime);

begin
  

  

  

  inherited; truncateRange();  
end;
class function TXQValue_gMonth.classTypeName: string;
begin
  result := 'gMonth';
end;
class function TXQValue_gMonth.createFromValue(const v: IXQValue): IXQValue;
begin
  
  
  result := inherited; ignore(v);
end;

class function TXQValue_gMonth.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_gMonth then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_gMonth.canCreateFromdatetime(const v: TXQ_datetime): boolean;
begin
  result := true; ignore(v);
  
  
  
  result:=inherited;
  

end;

class function TXQValue_gMonth.dateFormat(): string;
begin
  result := '--mm[Z]$';
end;












{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_gMonth);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type gDay. (automatic generated)
type TXQValue_gDay = class (TXQValue_datepart)
  constructor create(const v: TXQ_datetime); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromdatetime(const v: TXQ_datetime): boolean; override;
  
  class function dateFormat(): string; override;
  
  
  
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_gDay.create(const v: TXQ_datetime);

begin
  

  

  

  inherited; truncateRange();  
end;
class function TXQValue_gDay.classTypeName: string;
begin
  result := 'gDay';
end;
class function TXQValue_gDay.createFromValue(const v: IXQValue): IXQValue;
begin
  
  
  result := inherited; ignore(v);
end;

class function TXQValue_gDay.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_gDay then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_gDay.canCreateFromdatetime(const v: TXQ_datetime): boolean;
begin
  result := true; ignore(v);
  
  
  
  result:=inherited;
  

end;

class function TXQValue_gDay.dateFormat(): string;
begin
  result := '---dd[Z]$';
end;












{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_gDay);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type duration. (automatic generated)
type TXQValue_duration = class (TXQValueDateTime)
  constructor create(const v: TXQ_datetime); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
    class function classParentNonBlocked: TXQValueClass; override; 
  
  class function canCreateFromdatetime(const v: TXQ_datetime): boolean; override;
  
  class function dateFormat(): string; override;
  
  
  
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_duration.create(const v: TXQ_datetime);

begin
  

  

  

  inherited; truncateRange();  
end;
class function TXQValue_duration.classTypeName: string;
begin
  result := 'duration';
end;
class function TXQValue_duration.createFromValue(const v: IXQValue): IXQValue;
begin
  
  
  result := inherited; ignore(v);
end;

class function TXQValue_duration.classParentNonBlocked: TXQValueClass;
begin
  if classtype = TXQValue_duration then result := txqvalue_anyatomictype
  else result := TXQValueClass(classParent);
end;




class function TXQValue_duration.canCreateFromdatetime(const v: TXQ_datetime): boolean;
begin
  result := true; ignore(v);
  
  
  
  result:=inherited;
  

end;

class function TXQValue_duration.dateFormat(): string;
begin
  result := '[-]P[Y+"Y"][m+M][d+D][T[h+H][n+M][s+[.z+]S]]$';
end;












{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_duration);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type dayTimeDuration. (automatic generated)
type TXQValue_dayTimeDuration = class (TXQValue_duration)
  constructor create(const v: TXQ_datetime); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromdatetime(const v: TXQ_datetime): boolean; override;
  
  class function dateFormat(): string; override;
  
  
  procedure truncateRange();override; 
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_dayTimeDuration.create(const v: TXQ_datetime);

begin
  

  

  

  inherited; truncateRange();  
end;
class function TXQValue_dayTimeDuration.classTypeName: string;
begin
  result := 'dayTimeDuration';
end;
class function TXQValue_dayTimeDuration.createFromValue(const v: IXQValue): IXQValue;
begin
  
  
  result := inherited; ignore(v);
end;




class function TXQValue_dayTimeDuration.canCreateFromdatetime(const v: TXQ_datetime): boolean;
begin
  result := true; ignore(v);
  
  
  
  result:=inherited;
  

end;

class function TXQValue_dayTimeDuration.dateFormat(): string;
begin
  result := '[-]P[d+D][T[h+H][n+M][s+[.z+]S]]$';
end;

 procedure TXQValue_dayTimeDuration.truncateRange; begin value.year := 0; value.month := 0;  end; 










{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_dayTimeDuration);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





{$endif}








{$ifdef PXP_DERIVED_TYPES_INTERFACE}
//** Type for the XPath type yearMonthDuration. (automatic generated)
type TXQValue_yearMonthDuration = class (TXQValue_duration)
  constructor create(const v: TXQ_datetime); override;
  class function classTypeName: string; override;
  class function createFromValue(const v: IXQValue): IXQValue; override;
  
  
  class function canCreateFromdatetime(const v: TXQ_datetime): boolean; override;
  
  class function dateFormat(): string; override;
  
  
  procedure truncateRange();override; 
  
  
end;
{$endif}

{$ifdef PXP_DERIVED_TYPES_IMPLEMENTATION}


constructor TXQValue_yearMonthDuration.create(const v: TXQ_datetime);

begin
  

  

  

  inherited; truncateRange();  
end;
class function TXQValue_yearMonthDuration.classTypeName: string;
begin
  result := 'yearMonthDuration';
end;
class function TXQValue_yearMonthDuration.createFromValue(const v: IXQValue): IXQValue;
begin
  
  
  result := inherited; ignore(v);
end;




class function TXQValue_yearMonthDuration.canCreateFromdatetime(const v: TXQ_datetime): boolean;
begin
  result := true; ignore(v);
  
  
  
  result:=inherited;
  

end;

class function TXQValue_yearMonthDuration.dateFormat(): string;
begin
  result := '[-]P[Y+"Y"][m+M]$';
end;



 procedure TXQValue_yearMonthDuration.truncateRange; begin value.hour := 0; value.min := 0; value.sec := 0; end; 








{$endif}





{$ifdef PXP_DERIVED_TYPES_REGISTRATION}
TXQueryEngine.registerType(TXQValue_yearMonthDuration);
{$endif}

{$ifdef PXP_DERIVED_TYPES_FINALIZATION}

{$endif}

{$ifdef PXP_DERIVED_TYPES_UNITTESTS}





{$endif}






{$ifdef PXP_DERIVED_TYPES_UNITTESTS}
t('xs:unsignedByte(255)', '255', '');
t('xs:unsignedByte(256)', '0', '');
t('xs:unsignedByte(257)', '1', '');
t('xs:byte(255)', '127', '');
t('xs:byte(256)', '0', '');
t('xs:byte(257)', '1', '');
t('xs:byte(-1)', '-1', '');
t('xs:byte(-127)', '-127', '');
t('xs:byte(-128)', '-128', '');
t('xs:byte(-129)', '127', '');
t('xs:byte(-130)', '126', '');
t('xs:unsignedByte(-1)', '255', '');
t('xs:unsignedByte(-2)', '254', '');

t('type-of(xs:float(4.0))','float','');
t('type-of(xs:double(4.0))','double','');
t('type-of(xs:decimal(4.0))','decimal','');
t('type-of(4.0)','decimal','');
t('type-of(4.0e1)','double','');
t('type-of(4)','integer','');

t('xs:byte(1)+xs:byte(2)','3','');
t('type-of(xs:byte(1)+xs:byte(2))','byte','');
t('xs:short(1)-xs:short(2)','-1','');
t('type-of(xs:short(1)-xs:short(2))','short','');
t('xs:short(4)*xs:short(2)','8','');
t('type-of(xs:short(4)*xs:short(2))','short','');
t('xs:short(1) div xs:short(2)','0.5','');
t('type-of(xs:short(1) div xs:short(2))','decimal','');
t('xs:short(1) idiv xs:short(2)','0','');
t('type-of(xs:short(1) idiv xs:short(2))','short','');

//auto converting (mostly illegal in xpath, but who cares?)
t('xs:byte(1)+xs:short(2)','3','');
t('type-of(xs:byte(1)+xs:short(2))','short','');
t('xs:byte(1)+xs:unsignedShort(2)','3','');
t('type-of(xs:byte(1)+xs:unsignedShort(2))','integer','');
t('xs:byte(1)*xs:int(2)','2','');
t('type-of(xs:byte(1)*xs:int(2))','int','');
t('xs:byte(1)+2','3','');
t('type-of(xs:byte(1)+2)','integer','');


t('xs:float(1)+2','3','');
t('type-of(xs:float(1)+2)','float','');
t('xs:double(1)+2','3','');
t('type-of(xs:double(1)+2)','double','');
t('xs:decimal(1)+2','3','');
t('type-of(xs:decimal(1)+2)','decimal','');
t('xs:float(1)*xs:double(2)','2','');
t('type-of(xs:float(1)*xs:double(2))','double','');
t('xs:float(1)*2','2','');
t('type-of(xs:float(1)*2)','float','');
t('xs:float(1)*2.0','2','');
t('type-of(xs:float(1)*2.0)','float','');
t('xs:double(1)*2','2','');
t('type-of(xs:double(1)*2)','double','');
t('max((xs:float(1),xs:double(2),xs:decimal(3)))','3','');
t('type-of(max((xs:float(1),xs:double(2),xs:decimal(3))))','double','');
t('max((xs:float(1),xs:decimal(3)))','3','');
t('type-of(max((xs:float(1),xs:decimal(3))))','float','');
t('min((xs:float(1),xs:double(2),xs:decimal(3)))','1','');
t('type-of(min((xs:float(1),xs:double(2),xs:decimal(3))))','double','');
t('sum((xs:float(1),xs:decimal(3)))','4','');
t('type-of(sum((xs:float(1),xs:decimal(3))))','float','');

//instance of
t('xs:byte(1) instance of byte','true','');
t('xs:byte(1) instance of short','true','');
t('xs:byte(1) instance of integer','true','');
t('xs:byte(1) instance of decimal','true','');
t('xs:byte(1) instance of float','false','');
t('xs:byte(1) instance of double','false','');
t('xs:byte(1) instance of unsignedByte','false','');

t('xs:float(1) instance of float','true','');
t('xs:float(1) instance of double','false','');
t('xs:float(1) instance of decimal','false','');
t('xs:double(1) instance of float','false','');
t('xs:double(1) instance of double','true','');
t('xs:double(1) instance of decimal','false','');
t('xs:decimal(1) instance of float','false','');
t('xs:decimal(1) instance of double','false','');
t('xs:decimal(1) instance of decimal','true','');

t('xs:decimal(1) instance (: ... :) of decimal','true','');
t('xs:decimal(1) instance of (: .. :) decimal','true','');

t('xs:date("1800-02-03") instance of datetime','false','');
t('xs:time("23:59:59") instance of datetime','false','');

t('xs:untypedAtomic("abc") instance of string','false','');
t('string("def") instance of xs:untypedAtomic','false','');

//castable as
t('xs:byte(1) castable as byte','true','');
t('xs:byte(1) castable as integer','true','');
t('xs:byte(1) castable as float','true','');
t('xs:byte(1) castable as double','true','');
t('xs:byte(1) castable as unsignedByte','true','');
t('xs:byte(-1) castable as byte','true','');
t('xs:byte(-1) castable as integer','true','');
t('xs:byte(-1) castable as float','true','');
t('xs:byte(-1) castable as double','true','');
t('xs:byte(-1) castable as unsignedByte','false','');
t('xs:byte(-1) castable as unsignedByte','false','');
t('1.0 castable as positiveInteger','true','');
t('1.0e5 castable as positiveInteger','true','');
t('1.5 castable as positiveInteger','true','');
t('0.5 castable as positiveInteger','false','');
t('-1.0 castable as positiveInteger','false','');
t('-1.0 castable as integer','true','');
t('-1.2 castable as integer','true','');
t('-1.2 castable as boolean','true','');
t('5 castable as boolean','true','');
t('0 castable as boolean','true','');
t('"false" castable as boolean','true','');

t('xs:date("2050-01-01") castable as datetime','true','');
t('xs:time("00:00:00") castable as datetime','false','');
t('xs:datetime("1899-12-31T04:04:04") castable as date','true','');
t('xs:datetime("2012-02-01T14:15:16") castable as time','true','');
t('xs:datetime("1234-12-12T01:01:01") castable as time','true','');
t('xs:datetime("6543-12-12T08:08:08") castable as date','true','');
t('xs:date("1650-04-12") castable as time','false','');
t('xs:time("12:00:00") castable as date','false','');

t('xs:time("12:12:59") castable as untypedAtomic','true','');
t('xs:date("2000-01-01") castable as untypedAtomic','true','');
t('0.5 castable as untypedAtomic','true','');
t('xs:untypedAtomic("0.5") castable as float','true','');
t('xs:untypedAtomic("abc") castable as float','false','');
t('xs:untypedAtomic("abc") castable as string','true','');


t('xs:untypedAtomic("---15") castable as gDay','true','');
t('"---15" castable as gDay','true','');
t('"---15Z" castable as gDay','true','');
t('"---15+12:30" castable as gDay','true','');
t('"---15foobar" castable as gDay','false','');
t('"---5" castable as gDay','false',''); //true?
t('"---05" castable as gDay','true','');
t('"--12" castable as gMonth','true','');
t('"--13" castable as gMonth','false','');
t('"--12-10" castable as gMonth','true',''); //month in timezone -10
t('"--12-29" castable as gMonthDay','true','');
t('"--13-29" castable as gMonthDay','false','');
t('"2010" castable as gYear','true','');
t('"2010X" castable as gYear','false','');
t('"2010Z" castable as gYear','true',''); //2010 in UTC
t('"2010-12" castable as gYearMonth','true','');
t('"2010-13" castable as gYearMonth','false','');
t('"2010-12-10" castable as gYearMonth','true',''); //2010-12 in timezone -10
t('"2010-12+10" castable as gYearMonth','true',''); //2010 in UTC
t('type-of(xs:gYear("2010Z"))','gYear','');
t('type-of(xs:gMonth("--12-10"))','gMonth','');
t('xs:gMonth("--12")','--12','');
t('xs:gMonthDay("--12-03")','--12-03','');
t('xs:gDay("---30")','---30','');

t('xs:hexBinary("2020")','2020','');


//cast as
t('-1.2 cast as boolean','true','');
t('5 cast as boolean','true','');
t('0 cast  as boolean','false','');
t('"false" cast as boolean','false','');
t('xs:byte(-1) cast as double','-1','');

t('xs:time("23:45:59") cast as untypedAtomic','23:45:59','');
t('xs:date("1999-12-31") cast as untypedAtomic','1999-12-31','');
t('0.5 cast as untypedAtomic','0.5','');
t('xs:untypedAtomic("0.5") cast as float','0.5','');
t('xs:untypedAtomic("abc") cast as string','abc','');

t('type-of(xs:time("00:59:00") cast as untypedAtomic)','untypedAtomic','');
t('type-of(xs:date("2012-12-21") cast as untypedAtomic)','untypedAtomic','');
t('type-of(0.5 cast as untypedAtomic)','untypedAtomic','');
t('type-of(xs:untypedAtomic("0.5") cast as float)','float','');
t('type-of(xs:untypedAtomic("abc") cast as string)','string','');

{$endif}

{$undef PXP_DERIVED_TYPES_INTERFACE}
{$undef PXP_DERIVED_TYPES_IMPLEMENTATION}
{$undef PXP_DERIVED_TYPES_REGISTRATION}
{$undef PXP_DERIVED_TYPES_UNITTESTS}


