{**********************************************************************
                PilotLogic Software House.
  
 Package pl_VirtualTreeExtra.pkg
 This unit is part of CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit AllVTExtraRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LCLProc, LCLType, LMessages, LResources,
  LazIDEIntf, PropEdits, ComponentEditors,TypInfo,
  DB,
  vte_configtree,
  vte_json,
  //vte_rttigrid,
  vte_stringlist,
  vte_treedata,
  vte_propertytree,
  vte_buttontree,
  vte_initree,
  vte_edittree, vte_edittree_editors,

  vte_dbgrid,
  vte_dbcheckgroup,
  vte_dbtree,
  vte_dbtreeex,
  vte_ddbtreeview;

type


  TVTEditLinkProperty = class(TClassProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetEditLimit: Integer; override;
    function  GetValue: string; override;
    procedure SetValue(const Value: string); override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TVDDBDataFieldProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

{$R AllVTExtraRegister.res}
  
//============= TVDDBDataFieldProperty =============================
function TVDDBDataFieldProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TVDDBDataFieldProperty.GetValueList(List: TStrings);
var
  DBTreeFields: TVirtualDDBTreeFields;
  DataSource: TDataSource;
begin
  DBTreeFields := GetComponent(0) as TVirtualDDBTreeFields;
  DataSource := DBTreeFields.DBTreeView.DataSource;
  if DataSource <> nil then
    if DataSource.DataSet <> nil then
      DataSource.DataSet.GetFieldNames(List);
end;

procedure TVDDBDataFieldProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for i := 0 to Values.Count - 1 do Proc(Values[i]);
  finally
    Values.Free;
  end;
end;

//=================== TVTEditLinkProperty ===========================
function TVTEditLinkProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paSortList] - [paReadOnly, paSubProperties];
  if GetOrdValue <> 0 then Result := Result + [paValueList, paSortList, paSubProperties];
end;

function TVTEditLinkProperty.GetEditLimit: Integer;
begin
  Result := MaxIdentLength;
end;

function TVTEditLinkProperty.GetValue: string;
var
  C: TPersistent;
  P: PPropInfo;
begin
  C := GetComponent(0);
  P := TypInfo.GetPropInfo(C, 'EditLinkName', [tkLString, tkString, tkWString]);
  if Assigned(P) then
    Result := TypInfo.GetStrProp(C, P)
  else
    Result := '<not available>';
end;

procedure TVTEditLinkProperty.GetValues(Proc: TGetStrProc);
var
  L: TStringList;
  I: Integer;
begin
  L := TStringList.Create;
  try
    GetEditLinkClasses(L);
    for I := 0 to L.Count - 1 do
      Proc(L[I]);
  finally
    L.Free;
  end;
end;

procedure TVTEditLinkProperty.SetValue(const Value: string);
var
  C: TPersistent;
  P: PPropInfo;
  OldValue: string;
begin
  C := GetComponent(0);
  P := TypInfo.GetPropInfo(C, 'EditLinkName', [tkLString, tkString, tkWString]);
  if not Assigned(P) then Exit;
  OldValue := GetValue;

  TypInfo.SetStrProp(C, P, Value);
end;

//=============================================================
//=============================================================
//=============================================================

procedure Register;
begin

  RegisterComponents('Virtual Controls', [
                             TVirtualConfigTree,
                             TVirtualIniTree,
                             //TVirtualRttiGrid ,
                             TVirtualButtonTree,
                             TVirtualList,
                             TVirtualMemo,
                             TVirtualNumberedMemo,
                             TVirtualStringTreeData,
                             TVirtualPropertyTree,
                             TVirtualEditTree,
                             TVirtualJSONListView,
                             TVirtualJSONTreeView,
                             TVirtualJSONInspector
                             ]);

  RegisterComponents('Virtual Controls DB', [
                             TVirtualDbGrid,
                             TVirtualDBCheckGroup,
                             TVirtualDBTree, TDBCheckVirtualDBTree, TCheckVirtualDBTree,
                             TVirtualDBTreeEx, TDBCheckVirtualDBTreeEx, TCheckVirtualDBTreeEx,
                             TVirtualDDBTreeView
                             ]);

  RegisterPropertyEditor(TypeInfo(String), TVirtualDDBTreeFields, 'KeyFieldName', TVDDBDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String), TVirtualDDBTreeFields, 'ListFieldName', TVDDBDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String), TVirtualDDBTreeFields, 'ParentFieldName', TVDDBDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String), TVirtualDDBTreeFields, 'HasChildrenFieldName', TVDDBDataFieldProperty);

  RegisterPropertyEditor(TypeInfo(TEditLinkName), nil, 'EditLinkName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomEditLink), nil, '', TVTEditLinkProperty);

end;

end.

