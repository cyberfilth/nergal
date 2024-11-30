(* Connect to DeHashed & Hunter.io API's *)

unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, StdCtrls, ComCtrls, Menus, DOM, XMLRead, unit2,
  Classes, Dialogs, opensslsockets, fpjson, jsonparser, process, fphttpclient;

type

  { TForm1 }

  TForm1 = class(TForm)
    exportBtn2: TButton;
    hunterBtn: TButton;
    domainEdt2: TEdit;
    exportBtn: TButton;
    dehashedBtn: TButton;
    DomainCaption: TLabel;
    domainEdt: TEdit;
    DomainCaption2: TLabel;
    SaveDialog1: TSaveDialog;
    titleDeHashed: TLabel;
    titleHunter: TLabel;
    MainMenu1: TMainMenu;
    dehashedResults: TMemo;
    hunterResults: TMemo;
    MenuItem1: TMenuItem;
    mnuAbout: TMenuItem;
    mnuConfig: TMenuItem;
    mnuQuit: TMenuItem;
    PageControl1: TPageControl;
    DeHashed: TTabSheet;
    Hunter: TTabSheet;
    procedure dehashedBtnClick(Sender: TObject);
    procedure exportBtn2Click(Sender: TObject);
    procedure exportBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure hunterBtnClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuConfigClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
  private
    function FPHTTPClientDownload(URL: string; SaveToFile: boolean = False;
      Filename: string = ''): string;
  public
    procedure loadConfig;
    procedure connectToDeHashed;
    procedure connectToHunter;
  end;


var
  Form1: TForm1;
  hunterAPI, dehashedAPI, dehashedEmail, dfilename: string;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  (* Check to see if a config file exists *)
  if (FileExists(dfilename) = True) then
    loadConfig
  else
  begin
    try
      deetsForm.ShowModal();
    finally
      deetsForm.Free();
    end;
  end;
end;

procedure TForm1.hunterBtnClick(Sender: TObject);
begin
  (* Check to see if credentials have been saved *)
  if (hunterAPI = 'EMPTY') then
    ShowMessage('Unable to connect to Hunter.io' + sLineBreak +
      'Please enter your API key')
  (* Check if domain field is empty *)
  else if (domainEdt2.Text = '') then
    ShowMessage('Please enter a domain to search')
  else
    connectToHunter;
end;

procedure TForm1.mnuAboutClick(Sender: TObject);
begin
  ShowMessage(' -- Nergal -- ' + sLineBreak + sLineBreak + 'A GUI frontend for' +
    sLineBreak + 'Hunter.io &' + sLineBreak + 'DeHashed.' + sLineBreak +
    sLineBreak + 'Written in Free Pascal by' + sLineBreak + 'Chris Hawkins');
end;

procedure TForm1.mnuConfigClick(Sender: TObject);
begin
  try
    deetsForm.ShowModal();
  finally
    (* Reload credentials *)
    loadConfig;
  end;

end;

procedure TForm1.mnuQuitClick(Sender: TObject);
begin
  Close;
end;

function TForm1.FPHTTPClientDownload(URL: string; SaveToFile: boolean;
  Filename: string): string;
begin
  Result := '';
  with TFPHttpClient.Create(nil) do
    try
      try
        if SaveToFile then
        begin
          Get(URL, Filename);
          Result := Filename;
        end
        else
        begin
          Result := Get(URL);
        end;
      except
        on E: Exception do
          ShowMessage('Error: ' + E.Message);
      end;
    finally
      Free;
    end;
end;

procedure TForm1.dehashedBtnClick(Sender: TObject);
begin
  (* Check to see if credentials have been saved *)
  if (dehashedAPI = 'EMPTY') and (dehashedEmail = 'EMPTY') then
    ShowMessage('Unable to connect to DeHashed' + sLineBreak +
      'Please enter your email & API key')
  else if (dehashedAPI = 'EMPTY') then
    ShowMessage('Unable to connect to DeHashed' + sLineBreak +
      'Please enter your API key')
  else if (dehashedEmail = 'EMPTY') then
    ShowMessage('Unable to connect to DeHashed' + sLineBreak +
      'Please enter your email address')
  (* Check if domain field is empty *)
  else if (domainEdt.Text = '') then
    ShowMessage('Please enter a domain to search')
  else
    connectToDeHashed;
end;

(* Export Hunter.io results *)
procedure TForm1.exportBtn2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    hunterResults.Lines.SaveToFile(SaveDialog1.FileName)
  else
    Abort;
end;

(* Export DeHashed results *)
procedure TForm1.exportBtnClick(Sender: TObject);
begin
    if SaveDialog1.Execute then
      dehashedResults.Lines.SaveToFile(SaveDialog1.FileName)
  else
    Abort;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  dfilename := getUserDir + '.nergalConf.xml';
end;

procedure TForm1.loadConfig;
var
  RootNode, dehashedNode: TDOMNode;
  Doc: TXMLDocument;
begin
  try
    (* Read in xml file from disk *)
    ReadXMLFile(Doc, dfileName);
    (* Retrieve the hunter node *)
    RootNode := Doc.DocumentElement.FindNode('hunter');
    (* Retrieve the Hunter API *)
    hunterAPI := UTF8Encode(RootNode.FindNode('HUNTERAPI').TextContent);
    (* Retrieve the dehashed nodes *)
    dehashedNode := Doc.DocumentElement.FindNode('dehashed');
    (* Retrieve the Dehashed Email address *)
    dehashedEmail := UTF8Encode(dehashedNode.FindNode('DEHASHEDEMAIL').TextContent);
    (* Retrieve the Dehashed API *)
    dehashedAPI := UTF8Encode(dehashedNode.FindNode('DEHASHEDAPI').TextContent);
  finally
    (* free memory *)
    Doc.Free;
  end;
end;

procedure TForm1.connectToDeHashed;
var
  J: TJSONData;
  i, x: integer;
  s, fullQuery, emailList, prntEmail, prntPW, prntHash: string;
begin
  fullQuery := Concat('https://api.dehashed.com/search?query=username:',
    domainEdt.Text, ' -u ', dehashedEmail, ':', dehashedAPI,
    ' -H ''Accept: application/json''');
  {$IFDEF Linux}
  if RunCommand('/bin/bash', ['-c', 'curl ' + fullQuery], s) then
  {$ENDIF}
  {$IFDEF Darwin}
  if RunCommand('/usr/bin/curl ' + fullQuery, s) then
  {$ENDIF}
    try
      J := GetJSON(s);
      x := J.FindPath('total').AsInteger;
    except
      on E: Exception do
        ShowMessage('Error finding path!');
    end;

  emailList := '';
  dehashedResults.Lines.Clear;
  dehashedResults.Lines.Add('--- DeHashed results ---');

  for i := 0 to x - 1 do
  begin
    (* Initalise variables *)
    prntEmail := '';
    prntPW := '';
    prntHash := '';
    (* Only enumerate entries that contain an email address *)
    if (J.FindPath('entries[' + IntToStr(i) + '].email') <> nil) and
      (J.FindPath('entries[' + IntToStr(i) + '].email').AsString <> 'NULL') then
    begin
      (* Get email address *)
      prntEmail := J.FindPath('entries[' + IntToStr(i) + '].email').AsString;
      (* Get password *)
      if (J.FindPath('entries[' + IntToStr(i) + '].password') <> nil) and
        (J.FindPath('entries[' + IntToStr(i) + '].password').AsString <> '') then
        prntPW := J.FindPath('entries[' + IntToStr(i) + '].password').AsString;
      (* Get password hash *)
      if (J.FindPath('entries[' + IntToStr(i) + '].hashed_password') <> nil) and
        (J.FindPath('entries[' + IntToStr(i) + '].hashed_password').AsString <> '') then
        prntHash := J.FindPath('entries[' + IntToStr(i) + '].hashed_password').AsString;
      (* Print results to screen *)
      if (prntPW <> '') or (prntHash <> '') then
      begin
        (* new line *)
        dehashedResults.Lines.Add('');
        dehashedResults.Lines.Add(prntEmail);
        if (prntPW <> '') then
          dehashedResults.Lines.Add('Password: ' + prntPW);
        if (prntHash <> '') then
          dehashedResults.Lines.Add('Password hash: ' + prntHash);
      end
      else
        (* Add emails with no passwords or hashes to list *)
        emailList := emailList + prntEmail + sLineBreak;
      (* Reset variables *)
      prntEmail := '';
      prntPW := '';
      prntHash := '';
    end;
  end;
  (* Display emails with no passwords or hashes *)
  dehashedResults.Lines.Add(sLineBreak + '--- Emails with no associated passwords ---' +
    sLineBreak);
  dehashedResults.Lines.Add(emailList);
  (* Display save button *)
  exportBtn.Visible := True;
  (* Free JSON data *)
  J.Free;
end;

procedure TForm1.connectToHunter;
var
  J: TJSONData;
  i, x: integer;
  s, queryString: string;
begin
  queryString := 'https://api.hunter.io/v2/domain-search?domain=' +
    domainEdt2.Text + '&api_key=' + hunterAPI;
  s := FPHTTPClientDownload(queryString);
  try
    (* Parse JSON Data to TJSONData *)
    J := GetJSON(s);
    (* Get total number of records *)
    x := J.FindPath('meta.results').AsInteger;
  except
    on E: Exception do
      ShowMessage('Error parsing path');
  end;

  hunterResults.Lines.Clear;
  hunterResults.Lines.Add('--- Hunter.io results ---');

  for i := 0 to x - 1 do
  begin
    (* Only enumerate entries that contain an email address *)
    if (J.FindPath('data.emails[' + IntToStr(i) + '].value') <> nil) then
    begin
      (* new line *)
      hunterResults.Lines.Add('');
      (* Email address *)
      hunterResults.Lines.Add(J.FindPath('data.emails[' + IntToStr(i) +
        '].value').AsString);
      (* Name *)
      if (J.FindPath('data.emails[' + IntToStr(i) + '].first_name').IsNull =
        False) then
      begin
        if (J.FindPath('data.emails[' + IntToStr(i) + '].last_name').IsNull =
          False) then
          hunterResults.Lines.Add(J.FindPath('data.emails[' + IntToStr(i) +
            '].first_name').AsString + ' ' + J.FindPath(
            'data.emails[' + IntToStr(i) + '].last_name').AsString)
        else if (J.FindPath('data.emails[' + IntToStr(i) + '].last_name').IsNull =
          False) then
          hunterResults.Lines.Add(J.FindPath('data.emails[' + IntToStr(i) +
            '].first_name').AsString);
      end;
      (* Department *)
      if (J.FindPath('data.emails[' + IntToStr(i) + '].department').IsNull =
        False) then
        hunterResults.Lines.Add('Department: ' +
          J.FindPath('data.emails[' + IntToStr(i) + '].department').AsString);
    end;
  end;
  (* Display save button *)
  exportBtn2.Visible := True;
  (* Free JSON Data *)
  J.Free;
end;

end.
