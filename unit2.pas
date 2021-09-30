(* Store credentials *)

unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, ExtCtrls,
  StdCtrls, DOM, XMLWrite, XMLRead;

type

  { TdeetsForm }

  TdeetsForm = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    savebtn: TButton;
    dehashedEmailfield: TEdit;
    dehashedAPIfield: TEdit;
    hunterAPIfield: TEdit;
    hunterCaption: TLabel;
    dehashedCaption: TLabel;
    dehashedCaption2: TLabel;
    title: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure savebtnClick(Sender: TObject);
  private

  public

  end;

var
  deetsForm: TdeetsForm;

implementation

uses
  unit1;

{$R *.lfm}

{ TdeetsForm }

procedure TdeetsForm.FormCreate(Sender: TObject);
begin
  deetsForm.Caption := 'Enter your credentials';
end;

procedure TdeetsForm.FormShow(Sender: TObject);
var
  RootNode, dehashedNode: TDOMNode;
  Doc: TXMLDocument;
begin
  (* Check to see if a config file exists *)
  if (FileExists(unit1.dfilename) = True) then
  begin
    try
      (* Read in xml file from disk *)
      ReadXMLFile(Doc, unit1.dfilename);
      (* Retrieve the hunter node *)
      RootNode := Doc.DocumentElement.FindNode('hunter');
      (* Retrieve the Hunter API *)
      hunterAPIfield.Caption := UTF8Encode(RootNode.FindNode('HUNTERAPI').TextContent);
      (* Retrieve the dehashed nodes *)
      dehashedNode := Doc.DocumentElement.FindNode('dehashed');
      (* Retrieve the Dehashed Email address *)
      dehashedEmailfield.Caption :=
        UTF8Encode(dehashedNode.FindNode('DEHASHEDEMAIL').TextContent);
      (* Retrieve the Dehashed API *)
      dehashedAPIfield.Caption :=
        UTF8Encode(dehashedNode.FindNode('DEHASHEDAPI').TextContent);
    finally
      (* free memory *)
      Doc.Free;
    end;
  end;
end;

(* Store credentials in an external file *)
procedure TdeetsForm.savebtnClick(Sender: TObject);
var
  Doc: TXMLDocument;
  RootNode, dataNode: TDOMNode;

  procedure AddElement(Node: TDOMNode; Name, Value: UnicodeString);
  var
    NameNode, ValueNode: TDomNode;
  begin
    { creates future Node/Name }
    NameNode := Doc.CreateElement(Name);
    { creates future Node/Name/Value }
    ValueNode := Doc.CreateTextNode(Value);
    { place value in place }
    NameNode.Appendchild(ValueNode);
    { place Name in place }
    Node.Appendchild(NameNode);
  end;

  function AddChild(Node: TDOMNode; ChildName: shortstring): TDomNode;

  var
    ChildNode: TDomNode;
  begin
    ChildNode := Doc.CreateElement(UTF8Decode(ChildName));
    Node.AppendChild(ChildNode);
    Result := ChildNode;
  end;

begin
  try
    { Create a document }
    Doc := TXMLDocument.Create;
    { Create a root node }
    RootNode := Doc.CreateElement('root');
    Doc.Appendchild(RootNode);
    RootNode := Doc.DocumentElement;

    (* Hunter.io data *)
    DataNode := AddChild(RootNode, 'hunter');
    if (hunterAPIfield.Text <> '') then
      AddElement(datanode, 'HUNTERAPI', UTF8Decode(hunterAPIfield.Text))
    else
      AddElement(datanode, 'HUNTERAPI', 'EMPTY');

    (* Dehashed data *)
    DataNode := AddChild(RootNode, 'dehashed');
    if (dehashedEmailfield.Text <> '') then
      AddElement(datanode, 'DEHASHEDEMAIL', UTF8Decode(dehashedEmailfield.Text))
    else
      AddElement(datanode, 'DEHASHEDEMAIL', 'EMPTY');

    if (dehashedAPIfield.Text <> '') then
      AddElement(datanode, 'DEHASHEDAPI', UTF8Decode(dehashedAPIfield.Text))
    else
      AddElement(datanode, 'DEHASHEDAPI', 'EMPTY');

    (* Save XML file *)
    WriteXMLFile(Doc, unit1.dfilename);
  finally
    { free memory }
    Doc.Free;
  end;
  Close;
end;

end.
