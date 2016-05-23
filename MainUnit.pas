unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ShellApi, ComCtrls, ActnCtrls, FileCtrl, Registry, IniFiles, Menus, shlobj,

  GTAIV.TextureResource.Xbox360,
  GTAIV.TextureResource.PS3,

  Console.Xbox360.Graphics,
  Console.PS3.Graphics,

  Global.DirectDrawSurface,

  Compression.LZX,
  Compression.ZLib,

  About,
  Intro,
  TextureEditor.CPUSizeManual,

  ImagingTypes,
  Imaging,
  ImagingClasses,
  ImagingComponents,
  ImagingCanvases,
  ImagingFormats,
  ImagingUtility, ImgList, ToolWin, UI.Aero.Core.BaseControl,
  UI.Aero.Core.CustomControl, UI.Aero.PageManager, UI.Aero.Labels,
  UI.Aero.Core.CustomControl.Animation, UI.Aero.Button.Custom,
  UI.Aero.Button.Task, UI.Aero.Button, UI.Aero.Button.Theme,
  UI.Aero.black.GameButton, UI.Aero.ListBox,
  UI.Aero.BackForward, UI.Aero.Button.Expando, UI.Aero.Button.Extended,
  UI.Aero.Panel, Vcl.Imaging.pngimage, System.ImageList;
type
  TMainForm = class(TForm)
    btnExport: TAeroButton;
    Names: TListBox;
    dlgSaveImage: TSaveDialog;
    btnClose: TAeroButton;
    PaintBox: TPaintBox;
    ViewPanel: TPanel;
    cbxBackgroundMode: TComboBox;
    btnExportAll: TAeroButton;
    btnImport: TAeroButton;
    dlgImportImage: TOpenDialog;
    dlgFileOpen: TOpenDialog;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    tmiOpen: TMenuItem;
    lblWidth: TAeroLabel;
    lblWidthParam: TAeroLabel;
    lblHeight: TAeroLabel;
    lblHeightParam: TAeroLabel;
    lblTextureType: TAeroLabel;
    lblTextureTypeParam: TAeroLabel;
    lblOffset: TAeroLabel;
    lblOffsetParam: TAeroLabel;
    Options1: TMenuItem;
    Stretch: TMenuItem;
    N1: TMenuItem;
    tmiExit: TMenuItem;
    lblTextureName: TAeroLabel;
    btnFullScreen: TAeroButton;
    Help1: TMenuItem;
    About1: TMenuItem;
    Settings1: TMenuItem;
    tmiExport: TMenuItem;
    tmiImport: TMenuItem;
    N2: TMenuItem;
    tmiExportAll: TMenuItem;
    ToolBar: TToolBar;
    tbtnOpen: TToolButton;
    ImageList: TImageList;
    ToolButton2: TToolButton;
    tbtnExport: TToolButton;
    tbtnImport: TToolButton;
    tmiSave: TMenuItem;
    tmiClose: TMenuItem;
    ToolButton5: TToolButton;
    lblEndian: TAeroLabel;
    lblEndianParam: TAeroLabel;
    lblMipMaps: TAeroLabel;
    ToolButton6: TToolButton;
    tbtnSave: TToolButton;
    ImageTemp: TImage;
    cbMipMapsParam: TAeroPageManager;
    PagesLeft: TImage;
    PagesRight: TImage;
    procedure btnExportClick(Sender: TObject);
    procedure NamesClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ViewPanelResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnExportAllClick(Sender: TObject);
    procedure cbxBackgroundModeChange(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure tmiOpenClick(Sender: TObject);
    procedure StretchClick(Sender: TObject);
    procedure btnFullScreenClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure tmiCloseClick(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure tmiSaveClick(Sender: TObject);
    procedure cbMipMapsParamClick(Sender: TObject);
    procedure cbMipMapsParamButtonClick(Sender: TAeroPageManager;
      ButtonIndex: Integer);
  private
    FImage: ImagingClasses.TMultiImage;
    FImageCanvas: ImagingCanvases.TImagingCanvas;
    FBack: ImagingClasses.TSingleImage;
    FBackCanvas: ImagingCanvases.TImagingCanvas;
    FOriginalFormats: array of TImageFormat;
    SavedPos: TPoint;
    SavedSize: TSize;
  public
end;

var
  MainForm: TMainForm;

  lg_StartFolder: string;

  Stream, GfxStream: TMemoryStream;
  Settings: TIniFile;
  StretchParam: Boolean;
  ColorParam:Integer;

  tslWidths: TStringList;
  tslHeights: TStringList;
  tslMipmaps: TStringList;
  tslCurrentTexturePaths: TStringList;
  tslD3DBaseTextureOffsets: TStringList;
  tslTextureTypes: TStringList;
  tslEndians: TStringList;
  tslGPUTextureDataOffsets: TStringList;
  tslNameOffsets: TStringList;
  tslTextureListOffsets: TStringList;
  tslMipmapOffsets: TStringList;

  DictionaryName: string;
  OriginalPath: string;

  ResourceMemory: TMemoryStream;

  dwCPUSize, dwGPUSize: DWORD;
  dwWidth, dwHeight, dwMipMaps, dwOffset, dwEndian, dwTextureType: DWORD;
  isUnpacked: boolean;

  RunMode: integer;
  Reg: TRegistry;

procedure LoadFile(DicionaryName: String);

const
  FillColorLight  = $FF39DA35;
  FillColorGreen  = $FF008000;
  FillColorWhite  = $FFffffff;
  FillColorAero   = $FFA6CAF0;
  FillColorPink   = $FFf484b2;
  FillColorBlack  = $FF000000;
  FillColorGray   = $FFcfcfcf;
  FillColorYellow = $FFFFF001;

  BIF_NEWDIALOGSTYLE=$40;
  BIF_NONEWFOLDERBUTTON=$200;

implementation


{$R *.dfm}

function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if uMsg = BFFM_INITIALIZED then SendMessage(Wnd,BFFM_SETSELECTION, 1, Integer(@lg_StartFolder[1]));
  result := 0;
end;

function GetTempDir: String;
  function GetSpecialPath(CSIDL: word): string;
  var s:  string;
  begin
    SetLength(s, MAX_PATH);
    if not SHGetSpecialFolderPath(0, PChar(s), CSIDL, true)
    then s := GetSpecialPath(CSIDL_APPDATA);
    result := PChar(s);
  end;
begin
   Result:=GetSpecialPath(CSIDL_APPDATA);
end;

function BrowseForFolder(const browseTitle: string; const initialFolder: String =''; mayCreateNewFolder: Boolean = False): string;

var
  browse_info: TBrowseInfo;
  folder: array[0..MAX_PATH] of char;
  find_context: PItemIDList;
begin
  FillChar(browse_info,SizeOf(browse_info),#0);
  lg_StartFolder := initialFolder;
  browse_info.pszDisplayName := @folder[0];
  browse_info.lpszTitle := PChar(browseTitle);
  browse_info.ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE;
  if not mayCreateNewFolder then
    browse_info.ulFlags := browse_info.ulFlags or BIF_NONEWFOLDERBUTTON;
  browse_info.hwndOwner := Application.Handle;
  if initialFolder <> '' then
    browse_info.lpfn := BrowseForFolderCallBack;
  find_context := SHBrowseForFolder(browse_info);
  if Assigned(find_context) then
  begin
    if SHGetPathFromIDList(find_context,folder) then
      result := folder
    else
      result := '';
    GlobalFreePtr(find_context);
  end
  else
    result := '';
end;

procedure TMainForm.btnExportClick(Sender: TObject);
var
  S: string;
  dwStrLength: integer;
begin
 if Names.ItemIndex<>-1 then
    begin
      S:=MainForm.Names.Items[MainForm.Names.ItemIndex];
      dwStrLength:=length(S);
      if S[dwStrLength] = ')' then delete (S, dwStrLength-8, 9);
      MainForm.dlgSaveImage.FileName:=S;
      if cbMipMapsParam.CurrentPage=0 then MainForm.dlgSaveImage.FileName:=S
      else
        begin
          delete(S, pos('.dds', s), 4);
          S:=S+'_'+cbMipMapsParam.Pages[cbMipMapsParam.CurrentPage]+'.dds';
          MainForm.dlgSaveImage.FileName:=S;
        end;
      if MainForm.dlgSaveImage.Execute then
        begin
        if Pos('.c', DictionaryName)<>0 then
          begin
            ExportImagePS3(ResourceMemory, StrToInt(tslTextureTypes[Names.ItemIndex]), StrToInt(lblWidthParam.Caption), StrToInt(lblHeightParam.Caption), StrToInt(lblOffsetParam.Caption)+dwCPUSize, false, dlgSaveImage.FileName);
          end;
        if Pos('.x', DictionaryName)<>0 then
          begin
            ExportImageXbox360(ResourceMemory, StrToInt(tslTextureTypes[Names.ItemIndex]), StrToInt(lblWidthParam.Caption), StrToInt(lblHeightParam.Caption), StrToInt(tslEndians[Names.ItemIndex]), StrToInt(lblOffsetParam.Caption)+dwCPUSize, true, dlgSaveImage.FileName);
          end;
        end;
    end;
end;

procedure ViewImage(Path:String);
var
  S:string;
  I:Integer;
begin
  S:=path;
  if Assigned(Imaging.FindImageFileFormatByName(S)) then
    begin
      MainForm.FImage.LoadMultiFromFile(S);
      SetLength(MainForm.FOriginalFormats, MainForm.FImage.ImageCount);
      for I := 0 to MainForm.FImage.ImageCount - 1 do
        begin
          MainForm.FImage.ActiveImage := I;
          MainForm.FOriginalFormats[I] := MainForm.FImage.Format;
          if not (MainForm.FImage.Format in TImagingCanvas.GetSupportedFormats) then MainForm.FImage.Format := ifA8R8G8B8;
        end;
      MainForm.FImage.ActiveImage := 0;
    end;
  MainForm.PaintBox.Repaint;
end;


procedure TMainForm.btnExportAllClick(Sender: TObject);
var
  Dir, Dir1, S: string;
  i, j, dwStrLength:Integer;
begin
if Names.ItemIndex<>-1 then
  begin
    Dir := 'C:\';
    Dir:=BrowseForFolder('Export to:', '', true);
    for i := 0 to Names.Items.Count - 1do
      begin
        S:=MainForm.Names.Items[i];
        lblWidthParam.Caption:=tslWidths[i];
        lblHeightParam.Caption:=tslHeights[i];
        for j := 0 to StrToInt(tslMipMaps[i])-1 do
          cbMipMapsParam.Pages.Add(IntToStr(j));
        cbMipMapsParam.CurrentPage:=0;
        lblOffsetParam.Caption:=tslGPUTextureDataOffsets[i];
        lblTextureTypeParam.Caption:=tslTextureTypes[i];
        dwStrLength:=length(S);
        if S[dwStrLength] = ')' then  delete (S, dwStrLength-8, 9);
        Dir1:=Dir;
        Dir:= Dir+'\'+S;
        if Pos('.c', DictionaryName)<>0 then
            ExportImagePS3(ResourceMemory, StrToInt(tslTextureTypes[I]), StrToInt(tslWidths[I]), StrToInt(tslHeights[I]), StrToInt(tslGPUTextureDataOffsets[I])+dwCPUSize, false, Dir);
        if Pos('.x', DictionaryName)<>0 then
            ExportImageXbox360(ResourceMemory, StrToInt(tslTextureTypes[I]), StrToInt(tslWidths[I]), StrToInt(tslHeights[I]), StrToInt(tslEndians[I]), StrToInt(tslGPUTextureDataOffsets[I])+dwCPUSize, true, Dir);
        Dir:=Dir1;
      end;
  end;
end;

procedure TMainForm.btnImportClick(Sender: TObject);
var
  S: string;
  dwStrLength: integer;
begin
  if Names.ItemIndex>-1 then
    begin
      S:=MainForm.Names.Items[MainForm.Names.ItemIndex];
      dwStrLength:=length(S);
      if S[dwStrLength] = ')' then delete (S, dwStrLength-8, 9);
      if cbMipMapsParam.CurrentPage=0 then MainForm.dlgImportImage.FileName:=S
      else
        begin
          delete(S, pos('.dds', s), 4);
          S:=S+'_'+cbMipMapsParam.Pages[cbMipMapsParam.CurrentPage]+'.dds';
          MainForm.dlgImportImage.FileName:=S;
        end;
      if dlgImportImage.Execute then
        begin
          dwWidth:=StrToInt(lblWidthParam.Caption);
          dwHeight:=StrToInt(lblHeightParam.Caption);
          dwMipMaps:=StrToInt(cbMipMapsParam.Pages[cbMipMapsParam.CurrentPage])+1;
          dwOffset:=StrToInt(lblOffsetParam.Caption)+dwCPUSize;
          dwTextureType:=StrToInt(tslTextureTypes[Names.ItemIndex]);
          if Pos('.c', DictionaryName)<>0 then
            begin
              ImportImagePS3(ResourceMemory, StrToInt(tslTextureTypes[Names.ItemIndex]), dwWidth, dwHeight, dwOffset, false, dlgImportImage.FileName);
              ExportImagePS3(ResourceMemory, dwTextureType, dwWidth, dwHeight, dwOffset, false, ExtractFileDir(ParamStr(0))+'/'+S);
              ViewImage(ExtractFileDir(ParamStr(0))+'/'+S);
            end;
          if Pos('.x', DictionaryName)<>0 then
            begin
              dwEndian:=StrToInt(tslEndians[Names.ItemIndex]);
              ImportImageXbox360(ResourceMemory, StrToInt(tslTextureTypes[Names.ItemIndex]), dwWidth, dwHeight, dwEndian, dwOffset, true, dlgImportImage.FileName);
              ExportImageXbox360(ResourceMemory, dwTextureType, dwWidth, dwHeight, dwEndian, dwOffset, true, ExtractFileDir(ParamStr(0))+'/'+S);
              ViewImage(ExtractFileDir(ParamStr(0))+'/'+S);
            end;
        end;
    end;
end;

procedure TMainForm.NamesClick(Sender: TObject);
var
  i, j: Integer;
  S: string;
  dwStrLength: integer;
  dwMipMapOffset: DWORD;
begin
if Names.ItemIndex<>-1 then
  begin
    I:=Names.ItemIndex+1;
    lblWidthParam.Caption:=tslWidths[Names.ItemIndex];
    lblHeightParam.Caption:=tslHeights[Names.ItemIndex];
    MainForm.cbMipMapsParam.Pages.Clear;
    for j := 0 to StrToInt(tslMipMaps[Names.ItemIndex])-1 do
      cbMipMapsParam.Pages.Add(IntToStr(j));
    cbMipMapsParam.CurrentPage:=0;
    lblOffsetParam.Caption:='0x'+IntToHex(StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex]), 1);
    if tslCurrentTexturePaths.Count>0 then for I := 0 to tslCurrentTexturePaths.Count-1 do DeleteFile(tslCurrentTexturePaths[I]);
    if Pos('.x', DictionaryName)<>0 then
      begin
        lblTextureTypeParam.Caption:=Console.Xbox360.Graphics.GetGPUTEXTUREFORMAT(StrToInt(tslTextureTypes[Names.ItemIndex]));
        lblEndianParam.Caption:=Console.Xbox360.Graphics.GetGPUENDIAN(StrToInt(tslEndians[Names.ItemIndex]));
      end;
    if Pos('.c', DictionaryName)<>0 then
      begin
        lblTextureTypeParam.Caption:=Console.PS3.Graphics.GetGPUTEXTUREFORMAT(StrToInt(tslTextureTypes[Names.ItemIndex]));
      end;
    S:=MainForm.Names.Items[Names.ItemIndex];
    dwStrLength:=length(S);
    dwWidth:=StrToInt(tslWidths[Names.ItemIndex]);
    dwHeight:=StrToInt(tslHeights[Names.ItemIndex]);
    dwMipMaps:=StrToInt(cbMipMapsParam.Pages[cbMipMapsParam.CurrentPage])+1;
    dwOffset:=StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex]);
    if pos('.c', DictionaryName)<>0 then
      dwOffset:=GetMipOffsetPS3(dwTextureType, dwWidth, dwHeight, dwMipMaps, dwOffset);
    if pos('.x', DictionaryName)<>0 then
    begin
      dwMipMapOffset:=StrToInt(tslMipMapOffsets[Names.ItemIndex]);
      dwOffset:=GetMipOffsetXbox360(dwTextureType, dwWidth, dwHeight, dwMipMaps, dwOffset, dwMipMapOffset);
    end;
    dwOffset:=StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex])+dwCPUSize;
    dwTextureType:=StrToInt(tslTextureTypes[Names.ItemIndex]);
    if Pos('.x', DictionaryName)<>0 then
      begin
        dwEndian:=StrToInt(tslEndians[Names.ItemIndex]);
        ExportImageXbox360(ResourceMemory, dwTextureType, dwWidth, dwHeight, dwEndian, dwOffset, true, ExtractFileDir(ParamStr(0))+'/'+S);
      end;
    if Pos('.c', DictionaryName)<>0 then
      begin
        ExportImagePS3(ResourceMemory, dwTextureType, dwWidth, dwHeight, dwOffset, false, ExtractFileDir(ParamStr(0))+'/'+S);
      end;
    tslCurrentTexturePaths.Add(ExtractFileDir(ParamStr(0))+'/'+S);
    ViewImage(ExtractFileDir(ParamStr(0))+'/'+S);
  end;
end;

{Ñonsole resource loading procedure}

procedure SaveFile(DictionaryName: string);
begin
  if (isUnpacked) then
    begin
      ResourceMemory.Seek(dwCPUSize, 0);
      GfxStream.Seek(0, 0);
      GfxStream.CopyFrom(ResourceMemory, GfxStream.Size);
      GfxStream.SaveToFile(DictionaryName);
    end
  else
  begin
    if Pos('.x', DictionaryName)<>0 then    //work mode: Xbox360, compression algorythm: LZX
      begin
        GTAIV.TextureResource.Xbox360.SaveResource(Stream, ResourceMemory);
        Stream.SaveToFile(DictionaryName);
      end;
    if Pos('.c', DictionaryName)<>0 then    //work mode: PS3, compression algorythm: Zlib
      begin
        GTAIV.TextureResource.PS3.SaveResource(Stream, ResourceMemory);
        Stream.SaveToFile(DictionaryName);
      end;
  end;
end;

procedure LoadFile(DicionaryName: String);
var
  i, j, i1, dwStrLength: DWORD;
  s: string;
  bIsCPUSizeUnknown: boolean;
begin
  with MainForm do
    begin
    OriginalPath:=DictionaryName;
    isUnpacked:=false;
    PaintBox.Visible:=true;
    Stream.Free;
    Stream:=TMemoryStream.Create;
    Stream.LoadFromFile(DictionaryName);
    ResourceMemory.Free;
    ResourceMemory:=TMemoryStream.Create;
    MainForm.cbMipMapsParam.Pages.Clear;
    if tslCurrentTexturePaths.Count>0 then for I := 0 to tslCurrentTexturePaths.Count-1 do DeleteFile(tslCurrentTexturePaths[I]);
    if Pos('.sys', DictionaryName)<>0 then  // work mode: RAW resource
      begin
         isUnpacked:=true;
         if (pos('.x', DictionaryName)<>0) then  // Xbox360
           begin
             s:=DictionaryName;
             delete(s, Pos('.sys', s), 4);
             s:=s+'.gfx';
             OriginalPath:=s;
             s:=DictionaryName;
             for i:=1 to length(s) do if s[I]='\' then  i1:=I;
             Delete(s, 1, i1);
             MainForm.lblTextureName.Caption:=s;
             if (FileExists(DictionaryName)) then
               begin
                 delete(DictionaryName, Pos('.sys', DictionaryName), 4);
                 DictionaryName:=DictionaryName+'.gfx';
                 ResourceMemory.CopyFrom(Stream, Stream.Size);
                 GfxStream.Free;
                 GfxStream:=TMemoryStream.Create;
                 GfxStream.LoadFromFile(DictionaryName);
                 ResourceMemory.CopyFrom(GfxStream, GfxStream.Size);
                 dwCPUSize:=Stream.Size;
                 dwGPUSize:=GfxStream.Size;
                 if Pos('.xtd', DictionaryName)<>0  then LoadXTD(ResourceMemory, 0);
                 if Pos('.xhm', DictionaryName)<>0  then LoadXTD(ResourceMemory, 1);
                 if Pos('.xshp', DictionaryName)<>0  then LoadXTD(ResourceMemory, 2);
                 if Pos('.xsf', DictionaryName)<>0  then LoadXTD(ResourceMemory, 3);
                 if Pos('.xtx', DictionaryName)<>0  then LoadXTD(ResourceMemory, 4);
                 if Pos('.xvd', DictionaryName)<>0  then LoadXTD(ResourceMemory, 5);
                 if Pos('.xft', DictionaryName)<>0  then LoadXTD(ResourceMemory, 6);
                 Names.ItemIndex:=0;
                 if Names.ItemIndex<>-1 then
                   begin
                     I:=Names.ItemIndex+1;
                     lblWidthParam.Caption:=tslWidths[Names.ItemIndex];
                     lblHeightParam.Caption:=tslHeights[Names.ItemIndex];
                     for j := 0 to StrToInt(tslMipMaps[Names.ItemIndex])-1 do
                       cbMipMapsParam.Pages.Add(IntToStr(j));
                     cbMipMapsParam.CurrentPage:=0;
                     lblOffsetParam.Caption:='0x'+IntToHex(StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex]), 1);
                     lblTextureTypeParam.Caption:=Console.Xbox360.Graphics.GetGPUTEXTUREFORMAT(StrToInt(tslTextureTypes[Names.ItemIndex]));
                     lblEndianParam.Caption:=Console.Xbox360.Graphics.GetGPUENDIAN(StrToInt(tslEndians[Names.ItemIndex]));
                     S:=MainForm.Names.Items[Names.ItemIndex];
                     dwStrLength:=length(S);
                     {save texture and view it}
                     dwWidth:=StrToInt(tslWidths[Names.ItemIndex]);
                     dwHeight:=StrToInt(tslHeights[Names.ItemIndex]);
                     dwMipMaps:=StrToInt(cbMipMapsParam.Pages[cbMipMapsParam.CurrentPage])+1;
                     dwOffset:=StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex])+dwCPUSize;
                     dwEndian:=StrToInt(tslEndians[Names.ItemIndex]);
                     dwTextureType:=StrToInt(tslTextureTypes[Names.ItemIndex]);
                     ExportImageXbox360(ResourceMemory, dwTextureType, dwWidth, dwHeight, dwEndian, dwOffset, true, ExtractFileDir(ParamStr(0))+'/'+S);
                     tslCurrentTexturePaths.Add(ExtractFileDir(ParamStr(0))+'/'+S);
                     ViewImage(ExtractFileDir(ParamStr(0))+'/'+S);
                     tmiSave.Enabled:=true;
                     tmiExport.Enabled:=true;
                     tmiExportAll.Enabled:=true;
                     tmiImport.Enabled:=true;
                     tbtnSave.Enabled:=true;
                     tbtnExport.Enabled:=true;
                     tbtnImport.Enabled:=true;
                    end;
               end
               else
               begin
                 ShowMessage('*.gfx file not found!');
               end;
           end;
         if (pos('.c', DictionaryName)<>0) then  // PS3
           begin
             s:=DictionaryName;
             delete(s, Pos('.sys', s), 4);
             s:=s+'.gfx';
             OriginalPath:=s;
             s:=DictionaryName;
             for i:=1 to length(s) do if s[I]='\' then  i1:=I;
             Delete(s, 1, i1);
             MainForm.lblTextureName.Caption:=s;
             if (FileExists(DictionaryName)) then
               begin
                 delete(DictionaryName, Pos('.sys', DictionaryName), 4);
                 DictionaryName:=DictionaryName+'.gfx';
                 ResourceMemory.CopyFrom(Stream, Stream.Size);
                 GfxStream.Free;
                 GfxStream:=TMemoryStream.Create;
                 GfxStream.LoadFromFile(DictionaryName);
                 ResourceMemory.CopyFrom(GfxStream, GfxStream.Size);
                 dwCPUSize:=Stream.Size;
                 dwGPUSize:=GfxStream.Size;
                 if Pos('.ctd', DictionaryName)<>0  then LoadCTD(ResourceMemory, 0);
                 if Pos('.chm', DictionaryName)<>0  then LoadCTD(ResourceMemory, 1);
                 if Pos('.cshp', DictionaryName)<>0  then LoadCTD(ResourceMemory, 2);
                 if Pos('.csf', DictionaryName)<>0  then LoadCTD(ResourceMemory, 3);
                 if Pos('.ctx', DictionaryName)<>0  then LoadCTD(ResourceMemory, 4);
                 if Pos('.cvd', DictionaryName)<>0  then LoadCTD(ResourceMemory, 5);
                 if Pos('.cft', DictionaryName)<>0  then LoadCTD(ResourceMemory, 6);
                 Names.ItemIndex:=0;
                 if Names.ItemIndex<>-1 then
                   begin
                     I:=Names.ItemIndex+1;
                     lblWidthParam.Caption:=tslWidths[Names.ItemIndex];
                     lblHeightParam.Caption:=tslHeights[Names.ItemIndex];
                     for j := 0 to StrToInt(tslMipMaps[Names.ItemIndex])-1 do
                       cbMipMapsParam.Pages.Add(IntToStr(j));
                     cbMipMapsParam.CurrentPage:=0;
                     lblOffsetParam.Caption:='0x'+IntToHex(StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex]), 1);
                     lblTextureTypeParam.Caption:=Console.PS3.Graphics.GetGPUTEXTUREFORMAT(StrToInt(tslTextureTypes[Names.ItemIndex]));
                     lblEndianParam.Caption:='GPUENDIAN_NONE';
                     S:=MainForm.Names.Items[Names.ItemIndex];
                     dwStrLength:=length(S);
                     {save texture and view it}
                     dwWidth:=StrToInt(tslWidths[Names.ItemIndex]);
                     dwHeight:=StrToInt(tslHeights[Names.ItemIndex]);
                     dwMipMaps:=StrToInt(cbMipMapsParam.Pages[cbMipMapsParam.CurrentPage])+1;
                     dwOffset:=StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex])+dwCPUSize;
                     dwTextureType:=StrToInt(tslTextureTypes[Names.ItemIndex]);
                     ExportImagePS3(ResourceMemory, dwTextureType, dwWidth, dwHeight, dwOffset, false, ExtractFileDir(ParamStr(0))+'/'+S);
                     tslCurrentTexturePaths.Add(ExtractFileDir(ParamStr(0))+'/'+S);
                     ViewImage(ExtractFileDir(ParamStr(0))+'/'+S);
                     tmiSave.Enabled:=true;
                     tmiExport.Enabled:=true;
                     tmiExportAll.Enabled:=true;
                     tmiImport.Enabled:=true;
                     tbtnSave.Enabled:=true;
                     tbtnExport.Enabled:=true;
                     tbtnImport.Enabled:=true;
                   end;
               end;
             isUnpacked:=true;
           end;
      end;
    if not(isUnpacked) then
      begin
      if Pos('.x', DictionaryName)<>0 then    //work mode: Xbox360, decompression algorythm: LZX
        begin
          GTAIV.TextureResource.Xbox360.LoadResource(Stream, ResourceMemory, dwCPUSize, dwGPUSize, bIsCPUSizeUnknown);
          if bIsCPUSizeUnknown then
            begin
             FormCPUSizeManual.Position:=poOwnerFormCenter;
             FormCPUSizeManual.ShowModal;
            end;
          for I := 1 to length(DictionaryName) do if DictionaryName[I]='\' then  I1:=I;
          Delete(DictionaryName, 1, I1);
          MainForm.lblTextureName.Caption:=DictionaryName;
          if Pos('.xtd', DictionaryName)<>0 then LoadXTD(ResourceMemory, 0);
          if Pos('.xhm', DictionaryName)<>0 then LoadXTD(ResourceMemory, 1);
          if Pos('.xshp', DictionaryName)<>0 then LoadXTD(ResourceMemory, 2);
          if Pos('.xsf', DictionaryName)<>0  then LoadXTD(ResourceMemory, 3);
          if Pos('.xtx', DictionaryName)<>0  then LoadXTD(ResourceMemory, 4);
          if Pos('.xvd', DictionaryName)<>0  then LoadXTD(ResourceMemory, 5);
          if Pos('.xft', DictionaryName)<>0  then LoadXTD(ResourceMemory, 6);
          Names.ItemIndex:=0;
          if Names.ItemIndex<>-1 then
            begin
              I:=Names.ItemIndex+1;
              lblWidthParam.Caption:=tslWidths[Names.ItemIndex];
              lblHeightParam.Caption:=tslHeights[Names.ItemIndex];
              for j := 0 to StrToInt(tslMipMaps[Names.ItemIndex])-1 do
                cbMipMapsParam.Pages.Add(IntToStr(j));
              cbMipMapsParam.CurrentPage:=0;
              lblOffsetParam.Caption:='0x'+IntToHex(StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex]), 1);
              lblTextureTypeParam.Caption:=Console.Xbox360.Graphics.GetGPUTEXTUREFORMAT(StrToInt(tslTextureTypes[Names.ItemIndex]));
              lblEndianParam.Caption:=Console.Xbox360.Graphics.GetGPUENDIAN(StrToInt(tslEndians[Names.ItemIndex]));
              S:=MainForm.Names.Items[Names.ItemIndex];
              dwStrLength:=length(S);
              {save texture and view it}
              dwWidth:=StrToInt(tslWidths[Names.ItemIndex]);
              dwHeight:=StrToInt(tslHeights[Names.ItemIndex]);
              dwMipMaps:=StrToInt(cbMipMapsParam.Pages[cbMipMapsParam.CurrentPage])+1;
              dwOffset:=StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex])+dwCPUSize;
              dwEndian:=StrToInt(tslEndians[Names.ItemIndex]);
              dwTextureType:=StrToInt(tslTextureTypes[Names.ItemIndex]);
              ExportImageXbox360(ResourceMemory, dwTextureType, dwWidth, dwHeight, dwEndian, dwOffset, true, ExtractFileDir(ParamStr(0))+'/'+S);
              tslCurrentTexturePaths.Add(ExtractFileDir(ParamStr(0))+'/'+S);
              ViewImage(ExtractFileDir(ParamStr(0))+'/'+S);
              tmiSave.Enabled:=true;
              tmiExport.Enabled:=true;
              tmiExportAll.Enabled:=true;
              tmiImport.Enabled:=true;
              tbtnSave.Enabled:=true;
              tbtnExport.Enabled:=true;
              tbtnImport.Enabled:=true;
            end;
        end;
      if Pos('.c', DictionaryName)<>0 then    //work mode: PS3, decompression algorythm: Zlib
        begin
          GTAIV.TextureResource.PS3.LoadResource(Stream, ResourceMemory, dwCPUSize, dwGPUSize, bIsCPUSizeUnknown);
          if bIsCPUSizeUnknown then
            begin
             FormCPUSizeManual.Position:=poOwnerFormCenter;
             FormCPUSizeManual.ShowModal;
            end;
          for I := 1 to length(DictionaryName) do if DictionaryName[I]='\' then  I1:=I;
          Delete(DictionaryName, 1, I1);
          MainForm.lblTextureName.Caption:=DictionaryName;
          if Pos('.ctd', DictionaryName)<>0  then LoadCTD(ResourceMemory, 0);
          if Pos('.chm', DictionaryName)<>0  then LoadCTD(ResourceMemory, 1);
          if Pos('.cshp', DictionaryName)<>0  then LoadCTD(ResourceMemory, 2);
          if Pos('.csf', DictionaryName)<>0  then LoadCTD(ResourceMemory, 3);
          if Pos('.ctx', DictionaryName)<>0  then LoadCTD(ResourceMemory, 4);
          if Pos('.cvd', DictionaryName)<>0  then LoadCTD(ResourceMemory, 5);
          if Pos('.cft', DictionaryName)<>0  then LoadCTD(ResourceMemory, 6);
          Names.ItemIndex:=0;
          if Names.ItemIndex<>-1 then
            begin
              I:=Names.ItemIndex+1;
              lblWidthParam.Caption:=tslWidths[Names.ItemIndex];
              lblHeightParam.Caption:=tslHeights[Names.ItemIndex];
              lblOffsetParam.Caption:='0x'+IntToHex(StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex]), 1);
              lblTextureTypeParam.Caption:=Console.PS3.Graphics.GetGPUTEXTUREFORMAT(StrToInt(tslTextureTypes[Names.ItemIndex]));
              lblEndianParam.Caption:='GPUENDIAN_NONE';
              for j := 0 to StrToInt(tslMipMaps[Names.ItemIndex])-1 do
                cbMipMapsParam.Pages.Add(IntToStr(j));
              cbMipMapsParam.CurrentPage:=0;
              S:=MainForm.Names.Items[Names.ItemIndex];
              dwStrLength:=length(S);
              {save texture and view it}
              dwWidth:=StrToInt(tslWidths[Names.ItemIndex]);
              dwHeight:=StrToInt(tslHeights[Names.ItemIndex]);
              dwOffset:=StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex])+dwCPUSize;
              dwMipMaps:=StrToInt(cbMipMapsParam.Pages[cbMipMapsParam.CurrentPage])+1;
              dwTextureType:=StrToInt(tslTextureTypes[Names.ItemIndex]);
              ExportImagePS3(ResourceMemory, dwTextureType, dwWidth, dwHeight, dwOffset, false, ExtractFileDir(ParamStr(0))+'/'+S);
              tslCurrentTexturePaths.Add(ExtractFileDir(ParamStr(0))+'/'+S);
              ViewImage(ExtractFileDir(ParamStr(0))+'/'+S);
              tmiSave.Enabled:=true;
              tmiExport.Enabled:=true;
              tmiExportAll.Enabled:=true;
              tmiImport.Enabled:=true;
              tbtnSave.Enabled:=true;
              tbtnExport.Enabled:=true;
              tbtnImport.Enabled:=true;
            end;
        end;
      end;
    end;
end;


procedure TMainForm.tmiCloseClick(Sender: TObject);
begin
  Names.Clear;
  PaintBox.Visible:=false;
  lblTextureTypeParam.Caption:='';
  lblWidthParam.Caption:='';
  lblHeightParam.Caption:='';
  lblOffsetParam.Caption:='';
  lblTextureName.Caption:='';
  lblEndianParam.Caption:='';
  MainForm.cbMipMapsParam.Pages.Clear;
  tmiSave.Enabled:=false;
  tmiExport.Enabled:=false;
  tmiExportAll.Enabled:=false;
  tmiImport.Enabled:=false;
  tbtnSave.Enabled:=false;
  tbtnExport.Enabled:=false;
  tbtnImport.Enabled:=false;
end;

procedure TMainForm.tmiOpenClick(Sender: TObject);
begin
if dlgFileOpen.Execute then
  begin
    DictionaryName:=dlgFileOpen.FileName;
    LoadFile(DictionaryName);
  end;
end;


procedure TMainForm.tmiSaveClick(Sender: TObject);
begin
  SaveFile(OriginalPath);
end;

procedure TMainForm.ToolButton7Click(Sender: TObject);
begin

end;

{work procedures for tool interface, *.dds viewing, etc...}

procedure TMainForm.About1Click(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  close;
end;

procedure TMainForm.StretchClick(Sender: TObject);
begin
  if Stretch.Checked=false then Stretch.Checked:=true else Stretch.Checked:=false;
  if Stretch.Checked=true then StretchParam:=true else StretchParam:=false;
  Settings:=TiniFile.Create(extractfilepath(Application.ExeName)+'Settings.ini');
  Settings.WriteBool('Stretch','Enabled', StretchParam);
  Settings.Free;
  PaintBox.Repaint; //NOTE: can be buggy
end;

procedure Redraw;
var
  i, j: Integer;
  S: string;
  dwStrLength: integer;
  dwMipMapOffset: DWORD;
begin
with MainForm do
begin
if Names.ItemIndex<>-1 then
  begin
    I:=Names.ItemIndex+1;
    if tslCurrentTexturePaths.Count>0 then for I := 0 to tslCurrentTexturePaths.Count-1 do DeleteFile(tslCurrentTexturePaths[I]);
    if Pos('.x', DictionaryName)<>0 then
      begin
        lblTextureTypeParam.Caption:=Console.Xbox360.Graphics.GetGPUTEXTUREFORMAT(StrToInt(tslTextureTypes[Names.ItemIndex]));
        lblEndianParam.Caption:=Console.Xbox360.Graphics.GetGPUENDIAN(StrToInt(tslEndians[Names.ItemIndex]));
      end;
    if Pos('.c', DictionaryName)<>0 then
      begin
        lblTextureTypeParam.Caption:=Console.PS3.Graphics.GetGPUTEXTUREFORMAT(StrToInt(tslTextureTypes[Names.ItemIndex]));
      end;
    S:=MainForm.Names.Items[Names.ItemIndex];
    dwStrLength:=length(S);
    dwWidth:=StrToInt(tslWidths[Names.ItemIndex]);
    dwHeight:=StrToInt(tslHeights[Names.ItemIndex]);
    dwMipMaps:=StrToInt(cbMipMapsParam.Pages[cbMipMapsParam.CurrentPage])+1;
    dwOffset:=StrToInt(tslGPUTextureDataOffsets[Names.ItemIndex]);
    if pos('.c', DictionaryName)<>0 then
      dwOffset:=GetMipOffsetPS3(dwTextureType, dwWidth, dwHeight, dwMipMaps, dwOffset);
    if pos('.x', DictionaryName)<>0 then
    begin
      dwMipMapOffset:=StrToInt(tslMipMapOffsets[Names.ItemIndex]);
      dwOffset:=GetMipOffsetXbox360(dwTextureType, dwWidth, dwHeight, dwMipMaps, dwOffset, dwMipMapOffset);
    end;
    lblOffsetParam.Caption:='0x'+IntToHex(dwOffset, 1);
    dwOffset:=dwOffset++dwCPUSize;
    lblWidthParam.Caption:=IntToStr(dwWidth);
    lblHeightParam.Caption:=IntToStr(dwHeight);
    dwTextureType:=StrToInt(tslTextureTypes[Names.ItemIndex]);
    if Pos('.x', DictionaryName)<>0 then
      begin
        dwEndian:=StrToInt(tslEndians[Names.ItemIndex]);
        ExportImageXbox360(ResourceMemory, dwTextureType, dwWidth, dwHeight, dwEndian, dwOffset, true, ExtractFileDir(ParamStr(0))+'/'+S);
      end;
    if Pos('.c', DictionaryName)<>0 then
      begin
        ExportImagePS3(ResourceMemory, dwTextureType, dwWidth, dwHeight, dwOffset, false, ExtractFileDir(ParamStr(0))+'/'+S);
      end;
    tslCurrentTexturePaths.Add(ExtractFileDir(ParamStr(0))+'/'+S);
    ViewImage(ExtractFileDir(ParamStr(0))+'/'+S);
  end;
end;
end;

procedure TMainForm.cbMipMapsParamButtonClick(Sender: TAeroPageManager;
  ButtonIndex: Integer);
begin
  Redraw;
end;

procedure TMainForm.cbMipMapsParamClick(Sender: TObject);
begin
  Redraw;
end;

procedure TMainForm.cbxBackgroundModeChange(Sender: TObject);
begin
  case cbxBackgroundMode.ItemIndex of
    0:
    begin
      FBackCanvas.FillColor32 := FillColorWhite;
      ViewPanel.Color:= clWhite;
    end;
    1:
    begin
      FBackCanvas.FillColor32 := FillColorLight;
      ViewPanel.Color:=$0039DA35;
    end;
    2:
    begin
      FBackCanvas.FillColor32 := FillColorGreen;
      ViewPanel.Color:=clGreen;
    end;
    3:
    begin
      FBackCanvas.FillColor32 := FillColorAero;
      ViewPanel.Color:=$F0CAA6;
    end;
    4:
    begin
      FBackCanvas.FillColor32 := FillColorPink;
      ViewPanel.Color:=$b284f4;
    end;
    5:
    begin
      FBackCanvas.FillColor32 := FillColorBlack;
      ViewPanel.Color:=$000000;
    end;
    6:
    begin
      FBackCanvas.FillColor32 := FillColorGray;
      ViewPanel.Color:=$cfcfcf;
    end;
    7:
    begin
      FBackCanvas.FillColor32 := FillColorYellow;
      ViewPanel.Color:=$01F0FF;
    end;
  end;
  ColorParam:=cbxBackgroundMode.ItemIndex;
  Settings:=TiniFile.Create(extractfilepath(Application.ExeName)+'settings.ini');
  Settings.WriteInteger('Color','Number', ColorParam);
  Settings.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  S: string;
begin
  if (RunMode<>1) then
    begin
      PagesLeft.Picture.SaveToFile(GetTempDir+'\PagesLeft.png');
      PagesRight.Picture.SaveToFile(GetTempDir+'\PagesRight.png');
      Application.CreateForm(TIntroForm, IntroForm);
      Intro.IntroForm.ShowModal;
      Application.Terminate;
    end;

  tslWidths:=TStringList.Create;
  tslHeights:=TStringList.Create;
  tslMipmaps:=TStringList.Create;
  tslMipMapOffsets:=TStringList.Create;
  tslCurrentTexturePaths:=TStringList.Create;
  tslD3DBaseTextureOffsets:=TStringList.Create;
  tslTextureTypes:=TStringList.Create;
  tslEndians:=TStringList.Create;
  tslGPUTextureDataOffsets:=TStringList.Create;
  tslNameOffsets:=TStringList.Create;
  tslTextureListOffsets:=TStringList.Create;

  FImage := TMultiImage.Create;
  FImageCanvas := TImagingCanvas.Create;
  ImageTemp.Picture.SaveToFile(GetTempDir+'\TempImage.png');
  FImage.LoadFromFile(GetTempDir+'\TempImage.png');
  FBack := TSingleImage.CreateFromParams(2, 2, ifA8R8G8B8);
  FBackCanvas := TImagingCanvas.CreateForImage(FBack);
  FBackCanvas.FillColor32 := FillColorLight;
  FBackCanvas.FillRect(Rect(0, 0, 0, 0));
  PaintBox.Height:=100;

  Settings:=TiniFile.Create(extractfilepath(Application.ExeName)+'Settings.ini');
  StretchParam:=Settings.ReadBool('Stretch','Enabled', StretchParam);
  Width:=Settings.ReadInteger('Width', 'value', 1055);
  Height:=Settings.ReadInteger('Height', 'value', 700);
  Settings.Free;
  if StretchParam=true then Stretch.Checked:=true else Stretch.Checked:=false;
  Settings:=TiniFile.Create(extractfilepath(Application.ExeName)+'Settings.ini');
  ColorParam:=Settings.ReadInteger('Color','Number',ColorParam);
  try
    cbxBackgroundMode.ItemIndex:=ColorParam;
  except
    cbxBackgroundMode.ItemIndex:=1;
  end;
  Settings.Free;
  case cbxBackgroundMode.ItemIndex of
    0:
    begin
      FBackCanvas.FillColor32 := FillColorWhite;
      ViewPanel.Color:= clWhite;
    end;
    1:
    begin
      FBackCanvas.FillColor32 := FillColorLight;
      ViewPanel.Color:=$0039DA35;
    end;
    2:
    begin
      FBackCanvas.FillColor32 := FillColorGreen;
      ViewPanel.Color:=clGreen;
    end;
    3:
    begin
      FBackCanvas.FillColor32 := FillColorAero;
      ViewPanel.Color:=$F0CAA6;
    end;
    4:
    begin
      FBackCanvas.FillColor32 := FillColorPink;
      ViewPanel.Color:=$b284f4;
    end;
    5:
    begin
      FBackCanvas.FillColor32 := FillColorBlack;
      ViewPanel.Color:=$000000;
    end;
    6:
    begin
      FBackCanvas.FillColor32 := FillColorGray;
      ViewPanel.Color:=$cfcfcf;
    end;
    7:
    begin
      FBackCanvas.FillColor32 := FillColorYellow;
      ViewPanel.Color:=$01F0FF;
    end;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  I:Integer;
begin
  FImage.Free;
  FImageCanvas.Free;
  FBack.Free;
  FBackCanvas.Free;
  for I := 0 to tslCurrentTexturePaths.Count-1 do DeleteFile(tslCurrentTexturePaths[I]);
  Settings:=TiniFile.Create(extractfilepath(Application.ExeName)+'settings.ini');
  Settings.WriteInteger('Width', 'value', Width);
  Settings.WriteInteger('Height', 'value', Height);
  Settings.Free;
end;

procedure TMainForm.btnFullScreenClick(Sender: TObject);
begin
 if btnFullScreen.Tag = 0 then
  begin
   Self.Resizing(wsNormal);
   SavedPos.X:= Self.Left;
   SavedPos.Y:= Self.Top;
   SavedSize.cx:= Self.Width;
   SavedSize.cy:= Self.Height;
   Self.BorderStyle:= bsNone;
   Self.SetBounds(0,0,Screen.Width,Screen.Height+70);
   Self.BringToFront;
   btnFullScreen.Tag:= 1;
   btnFullScreen.Caption:= 'Windowed';
  end
 else
  begin
   Self.BorderStyle:= bsSizeable;
   Self.SetBounds(SavedPos.X,SavedPos.Y,SavedSize.cx,SavedSize.cy);
   Self.BringToFront;
   btnFullScreen.Tag:= 0;
   btnFullScreen.Caption:= 'Full screen';
  end;
end;

procedure TMainForm.PaintBoxPaint(Sender: TObject);
var
  R: TRect;
  Filter: TResizeFilter;
begin
  if (Stretch.Checked=true) then
  begin
      MainForm.PaintBox.Align:=AlClient;
  end
  else
    begin
    MainForm.PaintBox.Align:=AlNone;
    if (lblWidthParam.Caption<>'') and (lblHeightParam.Caption<>'') then
    begin
      MainForm.PaintBox.Width:=StrToInt(MainForm.lblWidthParam.Caption);
      MainForm.PaintBox.Height:=StrToInt(MainForm.lblHeightParam.Caption);
      FBack.Resize(PaintBox.ClientWidth, PaintBox.ClientHeight, rfNearest);
      FBackCanvas.UpdateCanvasState;
    end;
  end;
  FBackCanvas.FillRect(Rect(0, 0, FBack.Width, FBack.Height));
  R := ImagingUtility.ScaleRectToRect(FImage.BoundsRect, PaintBox.ClientRect);
  FImageCanvas.CreateForImage(FImage);
  FImage.ActiveImage := 1;
  Filter := rfBicubic;
  PaintBox.Color:=clNone;
  FImageCanvas.StretchDrawAlpha(FImage.BoundsRect, FBackCanvas, R, Filter);
  ImagingComponents.DisplayImage(PaintBox.Canvas, PaintBox.BoundsRect, FBack);
  FBack.Resize(PaintBox.ClientWidth, PaintBox.ClientHeight, rfNearest);
  FBackCanvas.UpdateCanvasState;
end;

procedure TMainForm.ViewPanelResize(Sender: TObject);
begin
  FBack.Resize(PaintBox.ClientWidth, PaintBox.ClientHeight, rfNearest);
  FBackCanvas.UpdateCanvasState;
end;

// some tip to save image resources (mipmap buttons)
initialization
begin
  Reg:=TRegistry.Create;
  with Reg do
    begin
      OpenKey('software', True);
      OpenKey('Dageron GTA V Console Texture Editor', True);
      if ValueExists('RunMode') then
        begin
          RunMode:=ReadInteger('RunMode');
          if (RunMode=0) then
            begin
             RunMode:=1;
             WriteInteger('RunMode', RunMode);
             RunMode:=0;
            end;
        end
        else
        begin
          RunMode:=1;
          WriteInteger('RunMode', RunMode);
          RunMode:=0;
        end;
      Free;
    end;
  if RunMode=1 then
    begin
      TAeroBasePageManager.ImageFile_Left:= GetTempDir+'\PagesLeft.png';
      TAeroBasePageManager.ImageFile_Right:= GetTempDir+'\PagesRight.png';
    end;
end

end.
