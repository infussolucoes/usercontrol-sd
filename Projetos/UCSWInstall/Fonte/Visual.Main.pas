{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usuários }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{
Autor da versão Original: Rodrigo Alves Cordeiro

Colaboradores da versão original
Alexandre Oliveira Campioni - alexandre.rural@netsite.com.br
Bernard Grandmougin
Carlos Guerra
Daniel Wszelaki
Everton Ramos [BS2 Internet]
Francisco Dueñas - fduenas@flashmail.com
Germán H. Cravero
Luciano Almeida Pimenta [ClubeDelphi.net]
Luiz Benevenuto - luiz@siffra.com
Luiz Fernando Severnini
Peter van Mierlo
Rodolfo Ferezin Moreira - rodolfo.fm@bol.com.br
Rodrigo Palhano (WertherOO)
Ronald Marconi
Sergiy Sekela (Dr.Web)
Stefan Nawrath
Vicente Barros Leonel [ Fknyght ]

*******************************************************************************}
{ Versão ShowDelphi Edition                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015   Giovani Da Cruz                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{ Você pode obter a última versão desse arquivo na pagina do projeto           }
{ User Control ShowDelphi Edition                                              }
{ Componentes localizado em http://infussolucoes.github.io/usercontrol-sd/     }
{                                                                              }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{                                                                              }
{ Comunidade Show Delphi - www.showdelphi.com.br                               }
{                                                                              }
{ Giovani Da Cruz  -  giovani@infus.inf.br  -  www.infus.inf.br                }
{                                                                              }
{ ****************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 01/07/2015: Gioavni Da Cruz
  |*  - Criação e distribuição da Primeira Versao ShowDelphi
  ******************************************************************************* }

unit Visual.Main;

interface

uses
  JclIDEUtils, JclCompilerUtils,

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvWizard, JvWizardRouteMapNodes, ShlObj,
  JvExControls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, uFrameLista,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage;

type
  TDestino = (tdSystem, tdDelphi, tdNone);

  TFrmPrincipal = class(TForm)
    wizPrincipal: TJvWizard;
    wizPgConfiguracao: TJvWizardInteriorPage;
    wizMapa: TJvWizardRouteMapNodes;
    Label2: TLabel;
    edtDirDestino: TEdit;
    btnSelecDirInstall: TSpeedButton;
    Label4: TLabel;
    edtDelphiVersion: TComboBox;
    Label5: TLabel;
    edtPlatform: TComboBox;
    wizPgPacote: TJvWizardInteriorPage;
    wizPgInstalacao: TJvWizardInteriorPage;
    btnInstalar: TButton;
    lstMsgInstalacao: TListBox;
    pgbInstalacao: TProgressBar;
    frameDpk: TframePacotes;
    ckbBCB: TCheckBox;
    Label8: TLabel;
    wizPgInicio: TJvWizardInteriorPage;
    Label6: TLabel;
    lblUrlForum1: TLabel;
    lblUrlACBr1: TLabel;
    Label19: TLabel;
    Label21: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label7: TLabel;
    wizPgFim: TJvWizardInteriorPage;
    Label3: TLabel;
    btnVisualizarLogCompilacao: TSpeedButton;
    pnlInfoCompilador: TPanel;
    lbInfo: TListBox;
    Label20: TLabel;
    Label1: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Image1: TImage;
    Label13: TLabel;
    Label14: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure edtDelphiVersionChange(Sender: TObject);
    procedure btnSelecDirInstallClick(Sender: TObject);
    procedure wizPgConfiguracaoNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure wizPgInstalacaoEnterPage(Sender: TObject;
      const FromPage: TJvWizardCustomPage);
    procedure btnInstalarClick(Sender: TObject);
    procedure btnVisualizarLogCompilacaoClick(Sender: TObject);
    procedure wizPgConfiguracaoCancelButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure wizPgConfiguracaoFinishButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure URLClick(Sender: TObject);
    procedure wizPgInstalacaoNextButtonClick(Sender: TObject;
      var Stop: Boolean);
  private
    FCountErros: Integer;
    oUserControl: TJclBorRADToolInstallations;
    iVersion: Integer;
    tPlatform: TJclBDSPlatform;
    sDirRoot: string;
    sDirLibrary: string;
    sDirPackage: string;
//    sDestino: TDestino;
    sPathBin: String;

    procedure BeforeExecute(Sender: TJclBorlandCommandLineTool);
    procedure OutputCallLine(const Text: string);
    function PathApp: String;
    function PathArquivoIni: String;
    function PathArquivoLog: String;
  public
    procedure GravarConfiguracoes;
    procedure LerConfiguracoes;

    procedure SetPlatformSelected;

    procedure AddLibrarySearchPath;
    procedure AddLibraryPathToDelphiPath(const APath: String;
      const AProcurarRemover: String);

    procedure ExtrairDiretorioPacote(NomePacote: string);

    procedure WriteToTXT(const ArqTXT, AString: AnsiString;
      const AppendIfExists: Boolean = True; AddLineBreak: Boolean = True);
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

{$R *.dfm}

uses
{$WARNINGS off} FileCtrl, {$WARNINGS on} ShellApi, IniFiles, StrUtils, Math,
  Registry;

procedure TFrmPrincipal.AddLibraryPathToDelphiPath(const APath,
  AProcurarRemover: String);
const
  cs: PChar = 'Environment Variables';
var
  lParam, wParam: Integer;
  aResult: Cardinal;
  ListaPaths: TStringList;
  I: Integer;
  PathsAtuais: String;
begin
  with oUserControl.Installations[iVersion] do
  begin
    // tentar ler o path configurado na ide do delphi, se não existir ler
    // a atual para complementar e fazer o override
    PathsAtuais := Trim(EnvironmentVariables.Values['PATH']);
    if PathsAtuais = '' then
      PathsAtuais := GetEnvironmentVariable('PATH');

    // manipular as strings
    ListaPaths := TStringList.Create;
    try
      ListaPaths.Delimiter := ';';
      ListaPaths.StrictDelimiter := True;
      ListaPaths.DelimitedText := PathsAtuais;

      // verificar se existe algo do ACBr e remover
      if Trim(AProcurarRemover) <> '' then
      begin
        for I := ListaPaths.Count - 1 downto 0 do
        begin
          if Pos(AnsiUpperCase(AProcurarRemover),
            AnsiUpperCase(ListaPaths[I])) > 0 then
            ListaPaths.Delete(I);
        end;
      end;

      // adicionar o path
      ListaPaths.Add(APath);

      // escrever a variavel no override da ide
      ConfigData.WriteString(cs, 'PATH', ListaPaths.DelimitedText);

      // enviar um broadcast de atualização para o windows
      wParam := 0;
      lParam := LongInt(cs);

      {$WARNINGS off}
      SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, wParam, lParam,
        SMTO_NORMAL, 4000, aResult);
      {$WARNINGS on}

      if aResult <> 0 then
        raise Exception.Create('Ocorreu um erro ao tentar configurar o path: ' +
          SysErrorMessage(aResult));
    finally
      ListaPaths.Free;
    end;
  end;

end;

procedure TFrmPrincipal.AddLibrarySearchPath;
  procedure FindDirs(ADirRoot: String);
  var
    oDirList: TSearchRec;
    iRet: Integer;
  begin
    ADirRoot := IncludeTrailingPathDelimiter(ADirRoot);

    iRet := FindFirst(ADirRoot + '*.*', faDirectory, oDirList);
    if iRet = 0 then
    begin
      try
        repeat
          if ((oDirList.Attr and faDirectory) <> 0) and (oDirList.Name <> '.')
            and (oDirList.Name <> '..') and (oDirList.Name <> '__history') then
          begin
            with oUserControl.Installations[iVersion] do
            begin
              AddToLibraryBrowsingPath(ADirRoot + oDirList.Name, tPlatform);
              //AddToLibrarySearchPath(ADirRoot + oDirList.Name, tPlatform);  //-< Há algum problema

              // RemoveFromLibrarySearchPath(ADirRoot + oDirList.Name, tPlatform);
            end;
            // -- Procura subpastas
            FindDirs(ADirRoot + oDirList.Name);
          end;
          iRet := FindNext(oDirList);
        until iRet <> 0;
      finally
        FindClose(oDirList)
      end;
    end;
  end;

begin
  FindDirs(IncludeTrailingPathDelimiter(sDirRoot) + 'Source');

  // --
  with oUserControl.Installations[iVersion] do
  begin
    AddToLibraryBrowsingPath(sDirLibrary, tPlatform);
    AddToLibrarySearchPath(sDirLibrary, tPlatform);
    AddToDebugDCUPath(sDirLibrary, tPlatform);
  end;

  // -- adicionar a library path ao path do windows
  AddLibraryPathToDelphiPath(sDirLibrary, 'UserControl');

  // -- ************ C++ Builder *************** //
  if ckbBCB.Checked then
  begin
    if oUserControl.Installations[iVersion] is TJclBDSInstallation then
    begin
      with TJclBDSInstallation(oUserControl.Installations[iVersion]) do
      begin
        AddToCppSearchPath(sDirLibrary, tPlatform);
        AddToCppLibraryPath(sDirLibrary, tPlatform);
        AddToCppBrowsingPath(sDirLibrary, tPlatform);
        AddToCppIncludePath(sDirLibrary, tPlatform);
      end;
    end;
  end;
end;

procedure TFrmPrincipal.BeforeExecute(Sender: TJclBorlandCommandLineTool);
begin
  // limpar os parâmetros do compilador
  Sender.Options.Clear;

  // não utilizar o dcc32.cfg
  if oUserControl.Installations[iVersion].SupportsNoConfig then
    Sender.Options.Add('--no-config');

  // -B = Build all units
  Sender.Options.Add('-B');
  // O+ = Optimization
  Sender.Options.Add('-$O-');
  // W- = Generate stack frames
  Sender.Options.Add('-$W+');
  // Y+ = Symbol reference info
  Sender.Options.Add('-$Y-');
  // -M = Make modified units
  Sender.Options.Add('-M');
  // -Q = Quiet compile
  Sender.Options.Add('-Q');
  // não mostrar warnings
  Sender.Options.Add('-H-');
  // não mostrar hints
  Sender.Options.Add('-W-');
  // -D<syms> = Define conditionals
  Sender.Options.Add('-DRELEASE');
  // -U<paths> = Unit directories
  // -U<paths> = Unit directories
  Sender.AddPathOption('U', oUserControl.Installations[iVersion].LibFolderName[tPlatform]);
  Sender.AddPathOption('U', oUserControl.Installations[iVersion].LibrarySearchPath[tPlatform]);
  Sender.AddPathOption('U', sDirLibrary);

  // -I<paths> = Include directories
  Sender.AddPathOption('I', oUserControl.Installations[iVersion].LibrarySearchPath[tPlatform]);

  // -R<paths> = Resource directories
  Sender.AddPathOption('R', oUserControl.Installations[iVersion].LibrarySearchPath
    [tPlatform]);


  // -N0<path> = unit .dcu output directory
  Sender.AddPathOption('N0', sDirLibrary);
  Sender.AddPathOption('LE', sDirLibrary);
  Sender.AddPathOption('LN', sDirLibrary);

  // ************ C++ Builder *************** //
  if ckbBCB.Checked then
  begin
    // -JL compila c++ builder
    Sender.AddPathOption('JL', sDirLibrary);
    // -NO compila .dpi output directory c++ builder
    Sender.AddPathOption('NO', sDirLibrary);
    // -NB compila .lib output directory c++ builder
    Sender.AddPathOption('NB', sDirLibrary);
    // -NH compila .hpp output directory c++ builder
    Sender.AddPathOption('NH', sDirLibrary);
  end;
  //
  with oUserControl.Installations[iVersion] do
  begin
    // -- Path para instalar os pacotes do Rave no D7, nas demais versões
    // -- o path existe.
    if VersionNumberStr = 'd7' then
      Sender.AddPathOption('U', oUserControl.Installations[iVersion].RootDir +
        '\Rave5\Lib');

    // -- Na versão XE2 por motivo da nova tecnologia FireMonkey, deve-se adicionar
    // -- os prefixos dos nomes, para identificar se será compilado para VCL ou FMX
    if VersionNumberStr = 'd16' then
      Sender.Options.Add
        ('-NSData.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win');

    if MatchText(VersionNumberStr, ['d17', 'd18', 'd19', 'd20', 'd21', 'd22'])
    then
      Sender.Options.Add
        ('-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell');

  end;
end;

procedure TFrmPrincipal.btnSelecDirInstallClick(Sender: TObject);
var
  Dir: String;
begin
  if SelectDirectory('Selecione o diretório de instalação', '', Dir,
    [sdNewFolder, sdNewUI, sdValidateDir]) then
    edtDirDestino.Text := Dir;
end;

procedure TFrmPrincipal.btnVisualizarLogCompilacaoClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(PathArquivoLog), '', '', 1);
end;

procedure TFrmPrincipal.btnInstalarClick(Sender: TObject);
var
  iDpk: Integer;
  bRunOnly: Boolean;
  NomePacote: String;
  Cabecalho: String;

  procedure MostrarMensagemInstalado(const aMensagem: String; const aErro: String = '');
  var
    Msg: String;
  begin
    WriteToTXT(AnsiString(PathArquivoLog), AnsiString(''));
    WriteToTXT(AnsiString(PathArquivoLog), Msg);

    lstMsgInstalacao.Items.Add(Msg);
    lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
  end;
begin

  FCountErros := 0;

  btnInstalar.Enabled := False;
  wizPgInstalacao.EnableButton(bkNext, False);
  wizPgInstalacao.EnableButton(bkBack, False);
  wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), False);

  try
    Cabecalho := 'Caminho: ' + edtDirDestino.Text + sLineBreak +
                 'Versão do delphi: ' + edtDelphiVersion.Text + ' (' + IntToStr(iVersion)+ ')' + sLineBreak +
                 'Plataforma: ' + edtPlatform.Text + '(' + IntToStr(Integer(tPlatform)) + ')' + sLineBreak +
                 StringOfChar('=', 80);

    // limpar o log
    lstMsgInstalacao.Clear;
    WriteToTXT(AnsiString(PathArquivoLog), AnsiString(Cabecalho), False);

    // setar barra de progresso
    pgbInstalacao.Position := 0;
    pgbInstalacao.Max := (frameDpk.Pacotes.Count * 2) + 2;

    // Seta a plataforna selecionada
    SetPlatformSelected;
    pgbInstalacao.Position := pgbInstalacao.Position + 1;
    lstMsgInstalacao.Items.Add('Setando parâmetros de plataforma...');
    Application.ProcessMessages;
    WriteToTXT(AnsiString(PathArquivoLog), AnsiString('Setando parâmetros de plataforma...'));

    // Adiciona os paths dos fontes na versão do delphi selecionada
    AddLibrarySearchPath;
    pgbInstalacao.Position := pgbInstalacao.Position + 1;
    lstMsgInstalacao.Items.Add('Adicionando library paths...');
    Application.ProcessMessages;
    WriteToTXT(AnsiString(PathArquivoLog), AnsiString('Adicionando library paths...'));

    // compilar os pacotes primeiramente
    lstMsgInstalacao.Items.Add('');
    lstMsgInstalacao.Items.Add('COMPILANDO OS PACOTES...');
    for iDpk := 0 to frameDpk.Pacotes.Count - 1 do
    begin
      NomePacote := frameDpk.Pacotes[iDpk].Caption;

      // Busca diretório do pacote
      ExtrairDiretorioPacote(NomePacote);

      if (IsDelphiPackage(NomePacote)) and (frameDpk.Pacotes[iDpk].Checked) then
      begin
        WriteToTXT(AnsiString(PathArquivoLog), AnsiString(''));

        if oUserControl.Installations[iVersion].CompilePackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
        begin
          lstMsgInstalacao.Items.Add(Format('Pacote "%s" compilado com sucesso.', [NomePacote]));
          lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
        end
        else
        begin
          Inc(FCountErros);
          lstMsgInstalacao.Items.Add(Format('Erro ao compilar o pacote "%s".', [NomePacote]));
          lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
        end;
      end;

      pgbInstalacao.Position := pgbInstalacao.Position + 1;
      Application.ProcessMessages;
    end;

    // instalar os pacotes somente se não ocorreu erro na compilação e plataforma for Win32
    if (edtPlatform.ItemIndex = 0) then
    begin
      if (FCountErros <= 0) then
      begin
        lstMsgInstalacao.Items.Add('');
        lstMsgInstalacao.Items.Add('INSTALANDO OS PACOTES...');
        lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;

        for iDpk := 0 to frameDpk.Pacotes.Count - 1 do
        begin
          NomePacote := frameDpk.Pacotes[iDpk].Caption;

          // Busca diretório do pacote
          ExtrairDiretorioPacote(NomePacote);

          if IsDelphiPackage(NomePacote) then
          begin
            // instalar somente os pacotes de designtime
            GetDPKFileInfo(sDirPackage + NomePacote, bRunOnly);
            if not bRunOnly then
            begin
              // se o pacote estiver marcado instalar, senão desinstalar
              if frameDpk.Pacotes[iDpk].Checked then
              begin
                WriteToTXT(AnsiString(PathArquivoLog), AnsiString(''));

                if oUserControl.Installations[iVersion].InstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
                begin
                  lstMsgInstalacao.Items.Add(Format('Pacote "%s" instalado com sucesso.', [NomePacote]));
                  lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
                end
                else
                begin
                  Inc(FCountErros);
                  lstMsgInstalacao.Items.Add(Format('Ocorreu um erro ao instalar o pacote "%s".', [NomePacote]));
                  lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
                end;
              end
              else
              begin
                WriteToTXT(AnsiString(PathArquivoLog), AnsiString(''));

                if oUserControl.Installations[iVersion].UninstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
                begin
                  lstMsgInstalacao.Items.Add(Format('Pacote "%s" removido com sucesso...', [NomePacote]));
                  lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
                end;
              end;
            end;
          end;

          pgbInstalacao.Position := pgbInstalacao.Position + 1;
          Application.ProcessMessages;
        end;
      end
      else
      begin
        lstMsgInstalacao.Items.Add('');
        lstMsgInstalacao.Items.Add('Abortando... Ocorreram erros na compilação dos pacotes.');
        lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
      end;
    end
    else
    begin
      lstMsgInstalacao.Items.Add('');
      lstMsgInstalacao.Items.Add('Para a plataforma de 64 bits os pacotes são somente compilados.');
      lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
    end;

  finally
    btnInstalar.Enabled := True;
    wizPgInstalacao.EnableButton(bkBack, True);
    wizPgInstalacao.EnableButton(bkNext, FCountErros = 0);
    wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), True);
  end;

  if FCountErros = 0 then
  begin
    pgbInstalacao.Position := pgbInstalacao.Max;
    Application.MessageBox(
      PWideChar(
        'Pacotes compilados e instalados com sucesso! '+sLineBreak+
        'Clique em "Próximo" para finalizar a instalação.'
      ),
      'Instalação',
      MB_ICONINFORMATION + MB_OK
    );
  end
  else
  begin
    if Application.MessageBox(
      PWideChar(
        'Ocorreram erros durante o processo de instalação, '+sLineBreak+
        'para maiores informações verifique o arquivo de log gerado.'+sLineBreak+sLineBreak+
        'Deseja visualizar o arquivo de log gerado?'
      ),
      'Instalação',
      MB_ICONQUESTION + MB_YESNO
    ) = ID_YES then
    begin
      btnVisualizarLogCompilacao.Click;
    end;
  end;
end;

procedure TFrmPrincipal.edtDelphiVersionChange(Sender: TObject);
begin
  iVersion := edtDelphiVersion.ItemIndex;
  sPathBin := IncludeTrailingPathDelimiter(oUserControl.Installations[iVersion]
    .BinFolderName);
  // -- Plataforma só habilita para Delphi XE2
  // -- Desabilita para versão diferente de Delphi XE2
  edtPlatform.Enabled := oUserControl.Installations[iVersion].VersionNumber >= 9;
  if oUserControl.Installations[iVersion].VersionNumber < 9 then
    edtPlatform.ItemIndex := 0;

  // C++ Builder a partir do D2006, versões anteriores tem IDE independentes.
  ckbBCB.Enabled := MatchText(oUserControl.Installations[iVersion].VersionNumberStr,
    ['d10', 'd11', 'd12', 'd14', 'd15', 'd16', 'd17', 'd18', 'd19', 'd20',
    'd21', 'd22', 'd23', 'd24']);
  if not ckbBCB.Enabled then
    ckbBCB.Checked := False;
end;

procedure TFrmPrincipal.ExtrairDiretorioPacote(NomePacote: string);
  procedure FindDirPackage(sDir, sPacote: String);
  var
    oDirList: TSearchRec;
    iRet: Integer;
  begin
    sDir := IncludeTrailingPathDelimiter(sDir);
    if not System.SysUtils.DirectoryExists(sDir) then
      Exit;

    iRet := FindFirst(sDir + '*.*', faAnyFile, oDirList);
    try
      while (iRet = 0) do
      begin
        iRet := FindNext(oDirList);
        if (oDirList.Name = '.') or (oDirList.Name = '..') or
          (oDirList.Name = '__history') then
        begin
          Continue;
        end;
        if oDirList.Attr = faDirectory then
          FindDirPackage(sDir + oDirList.Name, sPacote)
        else
        begin
          if oDirList.Name = sPacote then
          begin
            sDirPackage := IncludeTrailingPathDelimiter(sDir);
          end;
        end;
      end;
    finally
      FindClose(oDirList);
    end;
  end;

begin
  sDirPackage := '';

  FindDirPackage(sDirRoot + 'Packages\', NomePacote);
end;

procedure TFrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  oUserControl.Free;
end;

procedure TFrmPrincipal.FormCreate(Sender: TObject);
var
  iFor: Integer;
begin
  iVersion := -1;
  sDirRoot := '';
  sDirLibrary := '';
  sDirPackage := '';

  oUserControl := TJclBorRADToolInstallations.Create;

  // popular o combobox de versões do delphi instaladas na máquina
  for iFor := 0 to oUserControl.Count - 1 do
  begin
    if oUserControl.Installations[iFor].VersionNumberStr = 'd3' then
      edtDelphiVersion.Items.Add('Delphi 3')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd4' then
      edtDelphiVersion.Items.Add('Delphi 4')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd5' then
      edtDelphiVersion.Items.Add('Delphi 5')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd6' then
      edtDelphiVersion.Items.Add('Delphi 6')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd7' then
      edtDelphiVersion.Items.Add('Delphi 7')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd9' then
      edtDelphiVersion.Items.Add('Delphi 2005')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd10' then
      edtDelphiVersion.Items.Add('Delphi 2006')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd11' then
      edtDelphiVersion.Items.Add('Delphi 2007')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd12' then
      edtDelphiVersion.Items.Add('Delphi 2009')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd14' then
      edtDelphiVersion.Items.Add('Delphi 2010')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd15' then
      edtDelphiVersion.Items.Add('Delphi XE')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd16' then
      edtDelphiVersion.Items.Add('Delphi XE2')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd17' then
      edtDelphiVersion.Items.Add('Delphi XE3')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd18' then
      edtDelphiVersion.Items.Add('Delphi XE4')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd19' then
      edtDelphiVersion.Items.Add('Delphi XE5')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd20' then
      edtDelphiVersion.Items.Add('Delphi XE6')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd21' then
      edtDelphiVersion.Items.Add('Delphi XE7')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd22' then
      edtDelphiVersion.Items.Add('Delphi XE8')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd23' then
      edtDelphiVersion.Items.Add('Delphi XE9')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd24' then
      edtDelphiVersion.Items.Add('Delphi XE10');

    // -- Evento disparado antes de iniciar a execução do processo.
    oUserControl.Installations[iFor].DCC32.OnBeforeExecute := BeforeExecute;

    // -- Evento para saidas de mensagens.
    oUserControl.Installations[iFor].OutputCallback := OutputCallLine;
  end;

  if edtDelphiVersion.Items.Count > 0 then
  begin
    edtDelphiVersion.ItemIndex := 0;
    iVersion := 0;
  end;

  LerConfiguracoes;
end;

procedure TFrmPrincipal.GravarConfiguracoes;
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  try
    ArqIni.WriteString('CONFIG', 'DiretorioInstalacao', edtDirDestino.Text);
    ArqIni.WriteString('CONFIG', 'DelphiVersao', edtDelphiVersion.Text);
    ArqIni.WriteString('CONFIG', 'Plataforma', edtPlatform.Text);
    ArqIni.WriteBool('CONFIG', 'C++Builder', ckbBCB.Checked);

    for I := 0 to frameDpk.Pacotes.Count - 1 do
      ArqIni.WriteBool('PACOTES', frameDpk.Pacotes[I].Caption,
        frameDpk.Pacotes[I].Checked);
  finally
    ArqIni.Free;
  end;
end;

procedure TFrmPrincipal.wizPgConfiguracaoCancelButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  if Application.MessageBox(
    'Deseja realmente cancelar a instalação?',
    'Fechar',
    MB_ICONQUESTION + MB_YESNO
  ) = ID_YES then
  begin
    Self.Close;
  end;
end;

procedure TFrmPrincipal.wizPgConfiguracaoFinishButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  Self.Close;
end;

procedure TFrmPrincipal.wizPgConfiguracaoNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  // verificar se foi informado o diretório
  if Trim(edtDirDestino.Text) = EmptyStr then
  begin
    Stop := True;
    edtDirDestino.SetFocus;
    Application.MessageBox(
      'Diretório de instalação não foi informado.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  // prevenir versão do delphi em branco
  if Trim(edtDelphiVersion.Text) = '' then
  begin
    Stop := True;
    edtDelphiVersion.SetFocus;
    Application.MessageBox(
      'Versão do delphi não foi informada.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  // prevenir plataforma em branco
  if Trim(edtPlatform.Text) = '' then
  begin
    Stop := True;
    edtPlatform.SetFocus;
    Application.MessageBox(
      'Plataforma de compilação não foi informada.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  // Gravar as configurações em um .ini para utilizar depois
  GravarConfiguracoes;
end;

procedure TFrmPrincipal.wizPgInstalacaoEnterPage(Sender: TObject;
  const FromPage: TJvWizardCustomPage);
begin
  SetPlatformSelected;
  lstMsgInstalacao.Clear;
  pgbInstalacao.Position := 0;

  // mostrar ao usuário as informações de compilação
  with lbInfo.Items do
  begin
    Clear;
    Add(edtDelphiVersion.Text + ' ' + edtPlatform.Text);
    Add('Dir. Instalação  : ' + edtDirDestino.Text);
    Add('Dir. Bibliotecas : ' + sDirLibrary);
  end;

  // para 64 bit somente compilar
  if tPlatform = bpWin32 then // Win32
    btnInstalar.Caption := 'Instalar'
  else // win64
    btnInstalar.Caption := 'Compilar';
end;

procedure TFrmPrincipal.wizPgInstalacaoNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  if (lstMsgInstalacao.Count <= 0) then
  begin
    Stop := True;
    Application.MessageBox(
      'Clique no botão instalar antes de continuar.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  if (FCountErros > 0) then
  begin
    Stop := True;
    Application.MessageBox(
      'Ocorreram erros durante a compilação e instalação dos pacotes, verifique.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;
end;

procedure TFrmPrincipal.LerConfiguracoes;
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  try
    edtDirDestino.Text := ArqIni.ReadString('CONFIG', 'DiretorioInstalacao',
      ExtractFilePath(ParamStr(0)));
    edtPlatform.ItemIndex := edtPlatform.Items.IndexOf
      (ArqIni.ReadString('CONFIG', 'Plataforma', 'Win32'));
    edtDelphiVersion.ItemIndex := edtDelphiVersion.Items.IndexOf
      (ArqIni.ReadString('CONFIG', 'DelphiVersao', ''));
    ckbBCB.Checked := ArqIni.ReadBool('CONFIG', 'C++Builder', False);

    if Trim(edtDelphiVersion.Text) = '' then
      edtDelphiVersion.ItemIndex := 0;

    edtDelphiVersionChange(edtDelphiVersion);

    for I := 0 to frameDpk.Pacotes.Count - 1 do
      frameDpk.Pacotes[I].Checked := ArqIni.ReadBool('PACOTES',
        frameDpk.Pacotes[I].Caption, False);
  finally
    ArqIni.Free;
  end;
end;

// Evento disparado a cada ação do instalador
procedure TFrmPrincipal.OutputCallLine(const Text: string);
begin
  // remover a warnings de conversão de string (delphi 2010 em diante)
  // as diretivas -W e -H não removem estas mensagens
  if (Pos('Warning: W1057', Text) <= 0) and ((Pos('Warning: W1058', Text) <= 0))
  then
    WriteToTXT(AnsiString(PathArquivoLog), AnsiString(Text));
end;

// retornar o path do aplicativo
function TFrmPrincipal.PathApp: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

function TFrmPrincipal.PathArquivoIni: String;
var
  NomeApp: String;
begin
  NomeApp := ExtractFileName(ParamStr(0));
  Result := PathApp + ChangeFileExt(NomeApp, '.ini');
end;

// retornar o caminho completo para o arquivo de logs
function TFrmPrincipal.PathArquivoLog: String;
begin
  Result := PathApp + 'log_' + StringReplace(edtDelphiVersion.Text, ' ', '_',
    [rfReplaceAll]) + '.txt';
end;

procedure TFrmPrincipal.SetPlatformSelected;
var
  sVersao: String;
  sTipo: String;
begin
  iVersion := edtDelphiVersion.ItemIndex;
  sVersao := AnsiUpperCase(oUserControl.Installations[iVersion].VersionNumberStr);
  sDirRoot := IncludeTrailingPathDelimiter(edtDirDestino.Text);

  sTipo := 'Lib\Delphi\';

  if edtPlatform.ItemIndex = 0 then // Win32
  begin
    tPlatform := bpWin32;
    sDirLibrary := sDirRoot + sTipo + 'Lib' + sVersao;
  end
  else if edtPlatform.ItemIndex = 1 then // Win64
  begin
    tPlatform := bpWin64;
    sDirLibrary := sDirRoot + sTipo + 'Lib' + sVersao + 'x64';
  end;
end;

procedure TFrmPrincipal.URLClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(TLabel(Sender).Caption), '', '', 1);
end;

procedure TFrmPrincipal.WriteToTXT(const ArqTXT, AString: AnsiString;
  const AppendIfExists: Boolean; AddLineBreak: Boolean);
var
  FS: TFileStream;
  LineBreak: AnsiString;
begin
  FS := TFileStream.Create(string(ArqTXT), IfThen(AppendIfExists and
    System.SysUtils.FileExists(String(ArqTXT)),
    Integer(fmOpenReadWrite), Integer(fmCreate)) or fmShareDenyWrite);
  try
    FS.Seek(0, soFromEnd); // vai para EOF
    FS.Write(Pointer(AString)^, Length(AString));

    if AddLineBreak then
    begin
      LineBreak := sLineBreak;
      FS.Write(Pointer(LineBreak)^, Length(LineBreak));
    end;
  finally
    FS.Free;
  end;
end;

end.
