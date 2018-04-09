{**********************************************************************
 Package pl_VirtualTreeExtra.pkg
 This unit is part of CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit vte_configproviter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

type

  TvteConfig = class;

  { TvteConfigProvider }

  TvteConfigProvider = class(TComponent)
  protected
    function ReadInteger(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Integer; virtual;
    function ReadString(const SectionTitle, ItemKey: String; out ValueExists: Boolean): String; virtual; abstract;
    function ReadBoolean(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Boolean; virtual;
    function ReadFloat(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Double; virtual;
    procedure ReadSection(const SectionTitle: String; Strings: TStrings); virtual; abstract;
    procedure ReadSections(Strings: TStrings); virtual; abstract;
    procedure WriteInteger(const SectionTitle, ItemKey: String; AValue: Integer); virtual;
    procedure WriteString(const SectionTitle, ItemKey: String; AValue: String); virtual; abstract;
    procedure WriteBoolean(const SectionTitle, ItemKey: String; AValue: Boolean); virtual;
    procedure WriteFloat(const SectionTitle, ItemKey: String; AValue: Double); virtual;
    procedure Open; virtual; abstract;
    procedure Close; virtual; abstract;
  end;

  TvteConfigDataType = (ldtString, ldtInteger, ldtBoolean, ldtFloat);

  { TvteConfigItemDef }

  TvteConfigItemDef = class(TCollectionItem)
  private
    FDataType: TvteConfigDataType;
    FDefaultValue: String;
    FDisplayText: String;
    FKey: String;
    FSection: String;
    procedure SetDataType(const AValue: TvteConfigDataType);
    procedure SetDefaultValue(const AValue: String);
    procedure SetDisplayText(const AValue: String);
    procedure SetKey(const AValue: String);
    procedure SetSection(const AValue: String);
  protected
    function GetDisplayName: String; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DataType: TvteConfigDataType read FDataType write SetDataType;
    property DefaultValue: String read FDefaultValue write SetDefaultValue;
    property DisplayText: String read FDisplayText write SetDisplayText;
    property Key: String read FKey write SetKey;
    property Section: String read FSection write SetSection;
  end;

  { TvteConfigItemDefs }

  TvteConfigItemDefs = class(TCollection)
  private
    FOwner: TvteConfig;
    FHashList: TFPHashList;
    FValidHashList: Boolean;
    procedure BuildHashList;
    function GetItem(Index: Integer): TvteConfigItemDef;
  protected
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TvteConfig);
    destructor Destroy; override;
    function Add: TvteConfigItemDef;
    function Find(const Key: String): TvteConfigItemDef;
    function GetDefaultString(const Key: String): String;
    function GetDefaultInteger(const Key: String): Integer;
    function GetDefaultBoolean(const Key: String): Boolean;
    function GetDefaultFloat(const Key: String): Double;
    property Items[Index: Integer]: TvteConfigItemDef read GetItem; default;
  end;

  { TvteConfigSectionDef }

  TvteConfigSectionDef = class(TCollectionItem)
  private
    FDisplayText: String;
    FTitle: String;
    procedure SetDisplayTitle(const AValue: String);
    procedure SetTitle(const AValue: String);
  protected
    function GetDisplayName: String; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property DisplayText: String read FDisplayText write SetDisplayTitle;
    property Title: String read FTitle write SetTitle;
  end;

  { TvteConfigSectionDefs }

  TvteConfigSectionDefs = class(TCollection)
  private
    FOwner: TvteConfig;
    FHashList: TFPHashList;
    FValidHashList: Boolean;
    procedure BuildHashList;
    function GetItem(Index: Integer): TvteConfigSectionDef;
  protected
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem;
      Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TvteConfig);
    destructor Destroy; override;
    function Add: TvteConfigSectionDef;
    function Find(const Title: String): TvteConfigSectionDef;
    property Items[Index: Integer]: TvteConfigSectionDef read GetItem; default;
  end;

  TvteConfigNotificationType = (lcnOpen, lcnClose);

  ILuiConfigObserver = interface
    procedure ConfigNotification(NotificationType: TvteConfigNotificationType;
      Data: PtrInt);
  end;

  TvteConfigOption = (lcoParseMacros);

  TvteConfigOptions = set of TvteConfigOption;

  { TvteConfig }

  TvteConfig = class(TComponent)
  private
    FActive: Boolean;
    FDataProvider: TvteConfigProvider;
    FItemDefs: TvteConfigItemDefs;
    FObserverList: TFpList;
    FOptions: TvteConfigOptions;
    FSectionDefs: TvteConfigSectionDefs;
    procedure CheckObserverList;
    function HasObserver: Boolean;
    procedure InternalOpen;
    procedure InternalClose;
    procedure Notify(NotificationType: TvteConfigNotificationType; Data: PtrInt);
    procedure SetActive(const AValue: Boolean);
    procedure SetDataProvider(const AValue: TvteConfigProvider);
    procedure SetItemDefs(const AValue: TvteConfigItemDefs);
    procedure SetSectionDefs(const AValue: TvteConfigSectionDefs);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddObserver(Observer: ILuiConfigObserver);
    function GetItemText(const ItemKey: String; const SectionTitle: String = ''): String;
    function GetSectionText(const SectionTitle: String): String;
    function ReadInteger(const SectionTitle, ItemKey: String): Integer;
    function ReadString(const SectionTitle, ItemKey: String): String;
    function ReadBoolean(const SectionTitle, ItemKey: String): Boolean;
    function ReadFloat(const SectionTitle, ItemKey: String): Double;
    function ReadValue(const SectionTitle, ItemKey: String): Variant;
    procedure ReadSection(const SectionTitle: String; Strings: TStrings; RetrieveEmpty: Boolean = False);
    procedure ReadSections(Strings: TStrings; RetrieveEmpty: Boolean = False);
    procedure RemoveObserver(Observer: ILuiConfigObserver);
    procedure WriteInteger(const SectionTitle, ItemKey: String; AValue: Integer);
    procedure WriteString(const SectionTitle, ItemKey: String; AValue: String);
    procedure WriteBoolean(const SectionTitle, ItemKey: String; AValue: Boolean);
    procedure WriteFloat(const SectionTitle, ItemKey: String; AValue: Double);
    procedure WriteValue(const SectionTitle, ItemKey: String; AValue: Variant);
  published
    property Active: Boolean read FActive write SetActive;
    property DataProvider: TvteConfigProvider read FDataProvider write SetDataProvider;
    property ItemDefs: TvteConfigItemDefs read FItemDefs write SetItemDefs;
    property Options: TvteConfigOptions read FOptions write FOptions default [];
    property SectionDefs: TvteConfigSectionDefs read FSectionDefs write SetSectionDefs;
  end;

//todo: use miscutils instead
function ReplacePathMacros(const Path: String): String;
function ReplacePathMacrosUTF8(const Path: String): String;

implementation

uses
  StrUtils, Variants;

function ReplacePathMacros(const Path: String): String;
begin
  Result := AnsiReplaceText(Path, '$(APP_CONFIG_DIR)', GetAppConfigDir(False));
  Result := AnsiReplaceText(Result, '$(EXE_DIR)', ExtractFileDir(ParamStr(0)));
  Result := AnsiReplaceText(Result, '$(PATH_DELIM)', PathDelim);
end;

function ReplacePathMacrosUTF8(const Path: String): String;
begin
  Result := AnsiReplaceText(Path, '$(APP_CONFIG_DIR)', UTF8Encode(GetAppConfigDir(False)));
  Result := AnsiReplaceText(Result, '$(EXE_DIR)', ExtractFileDir(UTF8Encode(ParamStr(0))));
  Result := AnsiReplaceText(Result, '$(PATH_DELIM)', PathDelim);
end;


{ TvteConfig }

procedure TvteConfig.SetDataProvider(const AValue: TvteConfigProvider);
begin
  if FDataProvider = AValue then
    Exit;
  FDataProvider := AValue;
  if FDataProvider <> nil then
    FDataProvider.FreeNotification(Self);
end;

procedure TvteConfig.SetItemDefs(const AValue: TvteConfigItemDefs);
begin
  FItemDefs.Assign(AValue);
end;

procedure TvteConfig.SetSectionDefs(const AValue: TvteConfigSectionDefs);
begin
  FSectionDefs.Assign(AValue);
end;

procedure TvteConfig.CheckObserverList;
begin
  if FObserverList = nil then
    FObserverList := TFPList.Create;
end;

function TvteConfig.HasObserver: Boolean;
begin
  Result := (FObserverList <> nil) and (FObserverList.Count > 0);
end;

procedure TvteConfig.InternalOpen;
begin
  if FDataProvider = nil then
    raise Exception.Create('DataProvider not set');
  FDataProvider.Open;
  Notify(lcnOpen, 0);
end;

procedure TvteConfig.InternalClose;
begin
  if FDataProvider <> nil then
    FDataProvider.Close;
  Notify(lcnClose, 0);
end;

procedure TvteConfig.Loaded;
begin
  inherited Loaded;
  if FActive then
    InternalOpen;
end;

procedure TvteConfig.Notification(AComponent: TComponent; Operation: TOperation
  );
begin
  if (AComponent = FDataProvider) and (Operation = opRemove) then
  begin
    FDataProvider.RemoveFreeNotification(Self);
    FDataProvider := nil;
  end;
end;

procedure TvteConfig.Notify(NotificationType: TvteConfigNotificationType;
  Data: PtrInt);
var
  i: Integer;
begin
  if not HasObserver then
    Exit;
  for i := 0 to FObserverList.Count - 1 do
    ILuiConfigObserver(FObserverList[i]).ConfigNotification(NotificationType, Data);
end;

procedure TvteConfig.SetActive(const AValue: Boolean);
begin
  if FActive = AValue then
    Exit;
  FActive := AValue;
  if csLoading in ComponentState then
    Exit;
  if FActive then
    InternalOpen
  else
    InternalClose;
end;

constructor TvteConfig.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSectionDefs := TvteConfigSectionDefs.Create(Self);
  FItemDefs := TvteConfigItemDefs.Create(Self);
end;

destructor TvteConfig.Destroy;
begin
  InternalClose;
  FSectionDefs.Destroy;
  FItemDefs.Destroy;
  FObserverList.Free;
  inherited Destroy;
end;

procedure TvteConfig.AddObserver(Observer: ILuiConfigObserver);
begin
  CheckObserverList;
  FObserverList.Add(Observer);
end;

function TvteConfig.GetItemText(const ItemKey: String; const SectionTitle: String): String;
var
  Item: TvteConfigItemDef;
begin
  Item := FItemDefs.Find(ItemKey);
  if (Item <> nil) and (Item.Section = SectionTitle) then
    Result := Item.DisplayName
  else
    Result := ItemKey;
end;

function TvteConfig.GetSectionText(const SectionTitle: String): String;
var
  Section: TvteConfigSectionDef;
begin
  Section := FSectionDefs.Find(SectionTitle);
  if Section <> nil then
    Result := Section.DisplayName
  else
    Result := SectionTitle;
end;

function TvteConfig.ReadInteger(const SectionTitle, ItemKey: String): Integer;
var
  ValueExists: Boolean;
begin
  Result := FDataProvider.ReadInteger(SectionTitle, ItemKey, ValueExists);
  if not ValueExists then
    Result := FItemDefs.GetDefaultInteger(ItemKey);
end;

function TvteConfig.ReadString(const SectionTitle, ItemKey: String): String;
var
  ValueExists: Boolean;
begin
  Result := FDataProvider.ReadString(SectionTitle, ItemKey, ValueExists);
  if not ValueExists then
  begin
    Result := FItemDefs.GetDefaultString(ItemKey);
    if (Result <> '') and (lcoParseMacros in FOptions) then
      Result := ReplacePathMacrosUTF8(Result);
  end;
end;

function TvteConfig.ReadBoolean(const SectionTitle, ItemKey: String): Boolean;
var
  ValueExists: Boolean;
begin
  Result := FDataProvider.ReadBoolean(SectionTitle, ItemKey, ValueExists);
  if not ValueExists then
    Result := FItemDefs.GetDefaultBoolean(ItemKey);
end;

function TvteConfig.ReadFloat(const SectionTitle, ItemKey: String): Double;
var
  ValueExists: Boolean;
begin
  Result := FDataProvider.ReadFloat(SectionTitle, ItemKey, ValueExists);
  if not ValueExists then
    Result := FItemDefs.GetDefaultFloat(ItemKey);
end;

function TvteConfig.ReadValue(const SectionTitle, ItemKey: String): Variant;
var
  ValueExists: Boolean;
  Item: TvteConfigItemDef;
  S: String;
begin
  Item := FItemDefs.Find(ItemKey);
  if (Item <> nil) and (Item.DataType in [ldtBoolean, ldtFloat, ldtInteger]) then
  begin
    case Item.DataType of
     ldtBoolean:
       Result := DataProvider.ReadBoolean(SectionTitle, ItemKey, ValueExists);
     ldtInteger:
       Result := DataProvider.ReadInteger(SectionTitle, ItemKey, ValueExists);
     ldtFloat:
       Result := DataProvider.ReadFloat(SectionTitle, ItemKey, ValueExists);
    end;
    if not ValueExists then
      Result := Null;
  end
  else
  begin
    //fallback for string or special types that can be added in the future
    S := FDataProvider.ReadString(SectionTitle, ItemKey, ValueExists);
    if not ValueExists then
    begin
      if Item <> nil then
        S := Item.DefaultValue
      else
        S := FItemDefs.GetDefaultString(ItemKey);

      if S = '' then
        Result := Null
      else
      begin
        if lcoParseMacros in FOptions then
          S := ReplacePathMacrosUTF8(S);
        Result := S;
      end;
    end
    else
      Result := S;
  end;
end;

procedure TvteConfig.ReadSection(const SectionTitle: String; Strings: TStrings;
  RetrieveEmpty: Boolean);
var
  i, Last: Integer;
  Item: TvteConfigItemDef;
begin
  FDataProvider.ReadSection(SectionTitle, Strings);

  //Add the items that exists in ItemDefs but not in the provider
  if RetrieveEmpty then
  begin
    Last := 0;
    for i := 0 to FItemDefs.Count - 1 do
    begin
      Item := TvteConfigItemDef(FItemDefs.Items[i]);
      if ((Item.Section = '') or (Item.Section = SectionTitle)) and
        (Strings.IndexOf(Item.Key) = -1) then
      begin
        Strings.Insert(Last, Item.Key);
        Inc(Last);
      end;
    end;
  end;
end;

procedure TvteConfig.ReadSections(Strings: TStrings; RetrieveEmpty: Boolean);
var
  i, j, Last: Integer;
begin
  FDataProvider.ReadSections(Strings);
  if RetrieveEmpty then
  begin
    Last := 0;
    for i := 0 to FSectionDefs.Count - 1 do
    begin
      j := Strings.IndexOf(FSectionDefs[i].Title);
      if j = -1 then
      begin
        Strings.Insert(Last, FSectionDefs[i].Title);
        Inc(Last);
      end;
    end;
  end;
end;

procedure TvteConfig.RemoveObserver(Observer: ILuiConfigObserver);
begin
  if not HasObserver then
    Exit;
  FObserverList.Remove(Observer);
end;

procedure TvteConfig.WriteInteger(const SectionTitle, ItemKey: String; AValue: Integer);
begin
  FDataProvider.WriteInteger(SectionTitle, ItemKey, AValue);
end;

procedure TvteConfig.WriteString(const SectionTitle, ItemKey: String; AValue: String);
begin
  FDataProvider.WriteString(SectionTitle, ItemKey, AValue);
end;

procedure TvteConfig.WriteBoolean(const SectionTitle, ItemKey: String; AValue: Boolean);
begin
  FDataProvider.WriteBoolean(SectionTitle, ItemKey, AValue);
end;

procedure TvteConfig.WriteFloat(const SectionTitle, ItemKey: String; AValue: Double);
begin
  FDataProvider.WriteFloat(SectionTitle, ItemKey, AValue);
end;

procedure TvteConfig.WriteValue(const SectionTitle, ItemKey: String;
  AValue: Variant);
var
  Item: TvteConfigItemDef;
begin
  if VarIsNull(AValue) then
    FDataProvider.WriteString(SectionTitle, ItemKey, '')
  else
  begin
    Item := FItemDefs.Find(ItemKey);
    if (Item <> nil) and (Item.DataType in [ldtInteger, ldtBoolean, ldtFloat]) then
    begin
      case Item.DataType of
        ldtBoolean:
          FDataProvider.WriteBoolean(SectionTitle, ItemKey, AValue);
        ldtInteger:
          FDataProvider.WriteInteger(SectionTitle, ItemKey, AValue);
        ldtFloat:
          FDataProvider.WriteFloat(SectionTitle, ItemKey, AValue);
      end;
    end
    else
      FDataProvider.WriteString(SectionTitle, ItemKey, AValue);
  end;
end;

{ TvteConfigItemDef }

procedure TvteConfigItemDef.SetKey(const AValue: String);
begin
  if FKey=AValue then exit;
  FKey:=AValue;
end;

procedure TvteConfigItemDef.SetSection(const AValue: String);
begin
  if FSection=AValue then exit;
  FSection:=AValue;
end;

function TvteConfigItemDef.GetDisplayName: String;
begin
  if FDisplayText <> '' then
    Result := FDisplayText
  else
    Result := FKey;
end;

procedure TvteConfigItemDef.Assign(Source: TPersistent);
begin
  if Source is TvteConfigItemDef then
  begin
    Key := TvteConfigItemDef(Source).Key;
    DisplayText := TvteConfigItemDef(Source).DisplayText;
    DefaultValue := TvteConfigItemDef(Source).DefaultValue;
    DataType := TvteConfigItemDef(Source).DataType;
    Section := TvteConfigItemDef(Source).Section;
  end
  else
    inherited Assign(Source);
end;

procedure TvteConfigItemDef.SetDataType(const AValue: TvteConfigDataType);
begin
  if FDataType=AValue then exit;
  FDataType:=AValue;
end;

procedure TvteConfigItemDef.SetDefaultValue(const AValue: String);
begin
  if FDefaultValue=AValue then exit;
  FDefaultValue:=AValue;
end;

procedure TvteConfigItemDef.SetDisplayText(const AValue: String);
begin
  if FDisplayText=AValue then exit;
  FDisplayText:=AValue;
end;

{ TvteConfigSectionDefs }

procedure TvteConfigSectionDefs.BuildHashList;
var
  i: Integer;
  Section: TvteConfigSectionDef;
begin
  if FHashList = nil then
    FHashList := TFPHashList.Create
  else
    FHashList.Clear;
  for i := 0 to Count - 1 do
  begin
    Section := TvteConfigSectionDef(Items[i]);
    FHashList.Add(Section.Title, Section);
  end;
  FValidHashList := True;
end;

function TvteConfigSectionDefs.GetItem(Index: Integer): TvteConfigSectionDef;
begin
  Result := TvteConfigSectionDef(inherited GetItem(Index));
end;

function TvteConfigSectionDefs.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TvteConfigSectionDefs.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  FValidHashList := False;
end;

constructor TvteConfigSectionDefs.Create(AOwner: TvteConfig);
begin
  inherited Create(TvteConfigSectionDef);
  FOwner := AOwner;
end;

destructor TvteConfigSectionDefs.Destroy;
begin
  FHashList.Free;
  inherited Destroy;
end;

function TvteConfigSectionDefs.Add: TvteConfigSectionDef;
begin
  Result := TvteConfigSectionDef(inherited Add);
end;

function TvteConfigSectionDefs.Find(const Title: String): TvteConfigSectionDef;
begin
  if not FValidHashList then
    BuildHashList;
  Result := TvteConfigSectionDef(FHashList.Find(Title));
end;

{ TvteConfigSectionDef }

procedure TvteConfigSectionDef.SetDisplayTitle(const AValue: String);
begin
  if FDisplayText=AValue then exit;
  FDisplayText:=AValue;
end;

procedure TvteConfigSectionDef.SetTitle(const AValue: String);
begin
  if FTitle=AValue then exit;
  FTitle:=AValue;
end;

function TvteConfigSectionDef.GetDisplayName: String;
begin
  if FDisplayText <> '' then
    Result := FDisplayText
  else
    Result := FTitle;
end;

procedure TvteConfigSectionDef.Assign(Source: TPersistent);
begin
  if Source is TvteConfigSectionDef then
  begin
    DisplayText := TvteConfigSectionDef(Source).DisplayText;
    Title := TvteConfigSectionDef(Source).Title;
  end
  else
    inherited Assign(Source);
end;

{ TvteConfigItemDefs }

procedure TvteConfigItemDefs.BuildHashList;
var
  i: Integer;
  Item: TvteConfigItemDef;
begin
  if FHashList = nil then
    FHashList := TFPHashList.Create
  else
    FHashList.Clear;
  for i := 0 to Count - 1 do
  begin
    Item := TvteConfigItemDef(Items[i]);
    FHashList.Add(Item.Key, Item);
  end;
  FValidHashList := True;
end;

function TvteConfigItemDefs.GetItem(Index: Integer): TvteConfigItemDef;
begin
  Result := TvteConfigItemDef(inherited GetItem(Index));
end;

function TvteConfigItemDefs.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TvteConfigItemDefs.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  FValidHashList := False;
end;

constructor TvteConfigItemDefs.Create(AOwner: TvteConfig);
begin
  inherited Create(TvteConfigItemDef);
  FOwner := AOwner;
end;

destructor TvteConfigItemDefs.Destroy;
begin
  FHashList.Free;
  inherited Destroy;
end;

function TvteConfigItemDefs.Add: TvteConfigItemDef;
begin
  //protect the case the user try to create a item twice
  Result := TvteConfigItemDef(inherited Add);
end;

function TvteConfigItemDefs.Find(const Key: String): TvteConfigItemDef;
begin
  if not FValidHashList then
    BuildHashList;
  Result := TvteConfigItemDef(FHashList.Find(Key));
end;

function TvteConfigItemDefs.GetDefaultString(const Key: String): String;
var
  Item: TvteConfigItemDef;
begin
  Item := Find(Key);
  if Item <> nil then
    Result := Item.DefaultValue
  else
    Result := '';
end;

function TvteConfigItemDefs.GetDefaultInteger(const Key: String): Integer;
begin
  Result := StrToIntDef(GetDefaultString(Key), 0);
end;

function TvteConfigItemDefs.GetDefaultBoolean(const Key: String): Boolean;
begin
  Result := StrToBoolDef(GetDefaultString(Key), False);
end;

function TvteConfigItemDefs.GetDefaultFloat(const Key: String): Double;
begin
  Result := StrToFloatDef(GetDefaultString(Key), 0);
end;

{ TvteConfigProvider }

function TvteConfigProvider.ReadInteger(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Integer;
begin
  Result := StrToIntDef(ReadString(SectionTitle, ItemKey, ValueExists), 0);
end;

function TvteConfigProvider.ReadBoolean(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Boolean;
begin
  Result := StrToBoolDef(ReadString(SectionTitle, ItemKey, ValueExists), False);
end;

function TvteConfigProvider.ReadFloat(const SectionTitle, ItemKey: String; out ValueExists: Boolean): Double;
begin
  Result := StrToFloatDef(ReadString(SectionTitle, ItemKey, ValueExists), 0);
end;

procedure TvteConfigProvider.WriteInteger(const SectionTitle, ItemKey: String;
  AValue: Integer);
begin
  WriteString(SectionTitle, ItemKey, IntToStr(AValue));
end;

procedure TvteConfigProvider.WriteBoolean(const SectionTitle, ItemKey: String;
  AValue: Boolean);
begin
  WriteString(SectionTitle, ItemKey, IfThen(AValue, '1', '0'));
end;

procedure TvteConfigProvider.WriteFloat(const SectionTitle, ItemKey: String;
  AValue: Double);
begin
  WriteString(SectionTitle, ItemKey, FloatToStr(AValue));
end;

end.

