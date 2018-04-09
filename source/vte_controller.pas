{**********************************************************************
 Package pl_VirtualTreeExtra.pkg
 This unit is part of CodeTyphon Studio  (http://www.pilotlogic.com/)
***********************************************************************}

unit vte_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VirtualTrees, Graphics;

type

  TVTEvent = (evGetText, evFocusChanged, evInitNode, evDrawText, evMeasureItem, evResize);
  TVTEvents = set of TVTEvent;
  
  { TCustomVirtualTreeController }

  TCustomVirtualTreeController = class(TComponent)
  private
    FNodeDataSize: Integer;
    //for now connect to only one tree
    FTree: TVirtualStringTree;
    FEvents: TVTEvents;
    procedure ConnectEvents;
    procedure DisconnectEvents;
    //event bridges
   procedure DrawTextEvent(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure FocusChangedEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure GetTextEvent(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String);
    procedure InitNodeEvent(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure MeasureItemEvent(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer);
    procedure SetNodeDataSize(const AValue: Integer);
    procedure SetTree(const Value: TVirtualStringTree);
  protected
    //abstract event handlers
    procedure DoDrawText(TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean); virtual; abstract;
    procedure DoFocusChanged(Node: PVirtualNode; Column: TColumnIndex); virtual; abstract;
    procedure DoGetText(Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: String); virtual; abstract;
    procedure DoInitNode(ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates); virtual; abstract;
    procedure DoMeasureItem(TargetCanvas: TCanvas; Node: PVirtualNode;
      var NodeHeight: Integer); virtual; abstract;
    procedure DoResize(Sender: TObject); virtual; abstract;
    //LCL methods
    procedure Loaded; override;
    //Specific methods
    procedure TreeChanged; virtual;
    //Specific properties
    property Events: TVTEvents read FEvents write FEvents;
    property NodeDataSize: Integer read FNodeDataSize write SetNodeDataSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Tree: TVirtualStringTree read FTree write SetTree;
  end;

implementation

type
  TVTAccess = class(TCustomVirtualStringTree)
  end;

{ TCustomVirtualTreeController }

constructor TCustomVirtualTreeController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TCustomVirtualTreeController.Destroy;
begin
  //todo: is necessary to handle the case when FTree is destroied before
  //  the controller
  if FTree <> nil then
    DisconnectEvents;
  inherited Destroy;
end;

procedure TCustomVirtualTreeController.ConnectEvents;
begin
  if evGetText in FEvents then
    TVTAccess(FTree).OnGetText := @GetTextEvent;
  if evFocusChanged in FEvents then
    TVTAccess(FTree).OnFocusChanged := @FocusChangedEvent;
  if evInitNode in FEvents then
    TVTAccess(FTree).OnInitNode := @InitNodeEvent;
  if evDrawText in FEvents then
    TVTAccess(FTree).OnDrawText := @DrawTextEvent;
  if evMeasureItem in FEvents then
    TVTAccess(FTree).OnMeasureItem := @MeasureItemEvent;
  if evResize in FEvents then
    TVTAccess(FTree).OnResize := @DoResize;
end;

procedure TCustomVirtualTreeController.DisconnectEvents;
begin
  if evGetText in FEvents then
    TVTAccess(FTree).OnGetText := nil;
  if evFocusChanged in FEvents then
    TVTAccess(FTree).OnFocusChanged := nil;
  if evInitNode in FEvents then
    TVTAccess(FTree).OnInitNode := nil;
  if evDrawText in FEvents then
    TVTAccess(FTree).OnDrawText := nil;
  if evMeasureItem in FEvents then
    TVTAccess(FTree).OnMeasureItem := nil;
  if evResize in FEvents then
    TVTAccess(FTree).OnResize := nil;
end;

procedure TCustomVirtualTreeController.DrawTextEvent(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
  DoDrawText(TargetCanvas, Node, Column, CellText, CellRect, DefaultDraw);
end;

procedure TCustomVirtualTreeController.FocusChangedEvent(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  DoFocusChanged(Node, Column);
end;

procedure TCustomVirtualTreeController.GetTextEvent(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
begin
  DoGetText(Node, Column, TextType, CellText);
end;

procedure TCustomVirtualTreeController.InitNodeEvent(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  DoInitNode(ParentNode, Node, InitialStates);
end;

procedure TCustomVirtualTreeController.MeasureItemEvent(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);
begin
  DoMeasureItem(TargetCanvas, Node, NodeHeight);
end;

procedure TCustomVirtualTreeController.SetNodeDataSize(const AValue: Integer);
begin
end;

procedure TCustomVirtualTreeController.SetTree(const Value: TVirtualStringTree);
begin
  if FTree <> nil then
    DisconnectEvents;
  FTree := Value;
  if (FTree <> nil) and not (csLoading in ComponentState) then
    ConnectEvents;
  TreeChanged;
end;

procedure TCustomVirtualTreeController.Loaded;
begin
  inherited Loaded;
  if FTree <> nil then
    ConnectEvents;
end;

procedure TCustomVirtualTreeController.TreeChanged;
begin
  //
end;

end.

