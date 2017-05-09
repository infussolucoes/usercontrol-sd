{ **************************************************************************** }
{ Projeto: Componentes User Control ShowDelphi Edition                         }
{ Biblioteca multiplataforma de componentes Delphi para o controle de usu�rios }
{                                                                              }
{ Baseado nos pacotes Open Source User Control 2.31 RC1                        }
{
Autor da vers�o Original: Rodrigo Alves Cordeiro

Colaboradores da vers�o original
Alexandre Oliveira Campioni - alexandre.rural@netsite.com.br
Bernard Grandmougin
Carlos Guerra
Daniel Wszelaki
Everton Ramos [BS2 Internet]
Francisco Due�as - fduenas@flashmail.com
Germ�n H. Cravero
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
{ Vers�o ShowDelphi Edition                                                    }
{                                                                              }
{ Direitos Autorais Reservados (c) 2015   Giovani Da Cruz                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{ Voc� pode obter a �ltima vers�o desse arquivo na pagina do projeto           }
{ User Control ShowDelphi Edition                                              }
{ Componentes localizado em http://infussolucoes.github.io/usercontrol-sd/     }
{                                                                              }
{ Esta biblioteca � software livre; voc� pode redistribu�-la e/ou modific�-la  }
{ sob os termos da Licen�a P�blica Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a vers�o 2.1 da Licen�a, ou (a seu crit�rio) }
{ qualquer vers�o posterior.                                                   }
{                                                                              }
{ Esta biblioteca � distribu�da na expectativa de que seja �til, por�m, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia impl�cita de COMERCIABILIDADE OU      }
{ ADEQUA��O A UMA FINALIDADE ESPEC�FICA. Consulte a Licen�a P�blica Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICEN�A.TXT ou LICENSE.TXT)              }
{                                                                              }
{ Voc� deve ter recebido uma c�pia da Licen�a P�blica Geral Menor do GNU junto }
{ com esta biblioteca; se n�o, escreva para a Free Software Foundation, Inc.,  }
{ no endere�o 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Voc� tamb�m pode obter uma copia da licen�a em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{                                                                              }
{ Comunidade Show Delphi - www.showdelphi.com.br                               }
{                                                                              }
{ Giovani Da Cruz  -  giovani@infus.inf.br  -  www.infus.inf.br                }
{                                                                              }
{ **************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 01/07/2015: Giovani Da Cruz
  |*  - Cria��o e distribui��o da Primeira Versao ShowDelphi
  |*
  |* 06/02/2016: Giovani Da Cruz
  |*  - Ajuste para a adi��o autom�tica no library do delphi.
  **************************************************************************** }

unit Visual.Main;

interface

uses
  JclIDEUtils, JclCompilerUtils,

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JvWizard, JvWizardRouteMapNodes, ShlObj,
  JvExControls, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, uFrameLista,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage, JvComponentBase, JvCreateProcess;

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
    lstMsgInstalacao: TListBox;
    pgbInstalacao: TProgressBar;
    frameDpk: TframePacotes;
    ckbBCB: TCheckBox;
    Label8: TLabel;
    wizPgInicio: TJvWizardInteriorPage;
    Label6: TLabel;
    lblUrlForum1: TLabel;
    lblUrlUserControl1: TLabel;
    Label19: TLabel;
    Label21: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label7: TLabel;
    wizPgFim: TJvWizardInteriorPage;
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
    Label15: TLabel;
    pnlTopo: TPanel;
    Label16: TLabel;
    Label17: TLabel;
    Image2: TImage;
    JvCreateProcess1: TJvCreateProcess;
    Label18: TLabel;
    chkDeixarSomenteLIB: TCheckBox;
    btnInstalar: TButton;
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
    sDestino   : TDestino;
    sPathBin   : String;
    procedure BeforeExecute(Sender: TJclBorlandCommandLineTool);
    procedure AddLibrarySearchPath;
    procedure OutputCallLine(const Text: string);
    procedure SetPlatformSelected;
    function IsCheckOutJaFeito(const ADiretorio: String): Boolean;
    procedure CreateDirectoryLibrarysNotExist;
    procedure GravarConfiguracoes;
    procedure LerConfiguracoes;
    function PathApp: String;
    function PathArquivoIni: String;
    function PathArquivoLog: String;
    procedure ExtrairDiretorioPacote(NomePacote: string);
    procedure AddLibraryPathToDelphiPath(const APath: String;
      const AProcurarRemover: String);
    procedure FindDirs(ADirRoot: String; bAdicionar: Boolean = True);
    procedure DeixarSomenteLib;
    function RunAsAdminAndWaitForCompletion(hWnd: HWND; filename: string): Boolean;
    procedure WriteToTXT(const ArqTXT, AString: AnsiString;
      const AppendIfExists: Boolean = True; AddLineBreak: Boolean = True);
  public
  end;

var
  FrmPrincipal: TFrmPrincipal;

implementation

{$R *.dfm}

uses
{$WARNINGS off} FileCtrl, {$WARNINGS on} ShellApi, IniFiles, StrUtils, Math,
  Registry, System.Types, System.IOUtils;

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
  PathFonte: string;
begin
  with oUserControl.Installations[iVersion] do
  begin
    // tentar ler o path configurado na ide do delphi, se n�o existir ler
    // a atual para complementar e fazer o override
    PathsAtuais := Trim(EnvironmentVariables.Values['PATH']);
    if PathsAtuais = '' then
      PathsAtuais := GetEnvironmentVariable('PATH');

    // manipular as strings
    ListaPaths := TStringList.Create;
    try
      ListaPaths.Clear;
      ListaPaths.Delimiter       := ';';
      ListaPaths.StrictDelimiter := True;
      ListaPaths.DelimitedText   := PathsAtuais;

      // verificar se existe algo do UserControl e remover do environment variable PATH do delphi
      if Trim(AProcurarRemover) <> '' then
      begin
        for I := ListaPaths.Count - 1 downto 0 do
        begin
         if Pos(AnsiUpperCase(AProcurarRemover), AnsiUpperCase(ListaPaths[I])) > 0 then
           ListaPaths.Delete(I);
        end;
      end;

      // adicionar o path
      ListaPaths.Add(APath);

      // escrever a variavel no override da ide
      ConfigData.WriteString(cs, 'PATH', ListaPaths.DelimitedText);

      // enviar um broadcast de atualiza��o para o windows
      wParam := 0;
      lParam := LongInt(cs);
      SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, wParam, lParam, SMTO_NORMAL, 4000, aResult);
      if aResult <> 0 then
        raise Exception.create('Ocorreu um erro ao tentar configurar o path: ' + SysErrorMessage(aResult));
    finally
      ListaPaths.Free;
    end;
  end;
end;

procedure TFrmPrincipal.AddLibrarySearchPath;
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

  //-- ************ C++ Builder *************** //
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
  // limpar os par�metros do compilador
  Sender.Options.Clear;

  // n�o utilizar o dcc32.cfg
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
  // n�o mostrar warnings
  Sender.Options.Add('-H-');
  // n�o mostrar hints
  Sender.Options.Add('-W-');
  // -D<syms> = Define conditionals
  Sender.Options.Add('-DRELEASE');
  // -U<paths> = Unit directories
  Sender.AddPathOption('U', oUserControl.Installations[iVersion].LibFolderName[tPlatform]);
  Sender.AddPathOption('U', oUserControl.Installations[iVersion].LibrarySearchPath[tPlatform]);
  Sender.AddPathOption('U', sDirLibrary);
  // -I<paths> = Include directories
  Sender.AddPathOption('I', oUserControl.Installations[iVersion].LibrarySearchPath[tPlatform]);
  // -R<paths> = Resource directories
  Sender.AddPathOption('R', oUserControl.Installations[iVersion].LibrarySearchPath[tPlatform]);
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
     // -- Path para instalar os pacotes do Rave no D7, nas demais vers�es
     // -- o path existe.
     if VersionNumberStr = 'd7' then
        Sender.AddPathOption('U', oUserControl.Installations[iVersion].RootDir + '\Rave5\Lib');

     // -- Na vers�o XE2 por motivo da nova tecnologia FireMonkey, deve-se adicionar
     // -- os prefixos dos nomes, para identificar se ser� compilado para VCL ou FMX
     if VersionNumberStr = 'd16' then
        Sender.Options.Add('-NSData.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win');

     if MatchText(VersionNumberStr, ['d17','d18','d19','d20','d21','d22','d23','d24']) then
        Sender.Options.Add('-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;IBX');

  end;
end;

procedure TFrmPrincipal.btnSelecDirInstallClick(Sender: TObject);
var
  Dir: String;
begin
  if SelectDirectory('Selecione o diret�rio de instala��o', '', Dir,
    [sdNewFolder, sdNewUI, sdValidateDir]) then
    edtDirDestino.Text := Dir;
end;

procedure TFrmPrincipal.btnVisualizarLogCompilacaoClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(PathArquivoLog), '', '', 1);
end;

// cria��o dos diret�rios necess�rios
procedure TfrmPrincipal.CreateDirectoryLibrarysNotExist;
begin
  // Checa se existe diret�rio da plataforma
  if not DirectoryExists(sDirLibrary) then
    ForceDirectories(sDirLibrary);
end;

procedure TFrmPrincipal.DeixarSomenteLib;
procedure Copiar(const Extensao : string);
  var
    ListArquivos: TStringDynArray;
    Arquivo : string;
    i: integer;
  begin
    ListArquivos := TDirectory.GetFiles(IncludeTrailingPathDelimiter(sDirRoot) + 'Source', Extensao ,TSearchOption.soAllDirectories ) ;
    for i := Low(ListArquivos) to High(ListArquivos) do
    begin
      Arquivo := ExtractFileName(ListArquivos[i]);
      CopyFile(PWideChar(ListArquivos[i]), PWideChar(IncludeTrailingPathDelimiter(sDirLibrary) + Arquivo), True);
    end;
  end;
begin
  // remover os path com o segundo parametro
  FindDirs(IncludeTrailingPathDelimiter(sDirRoot) + 'Source', False);

  Copiar('*.dcr');
  Copiar('*.res');
  Copiar('*.dfm');
  Copiar('*.ini');
	Copiar('*.inc');
end;

procedure TFrmPrincipal.btnInstalarClick(Sender: TObject);
var
  iDpk: Integer;
  bRunOnly: Boolean;
  NomePacote: String;
  Cabecalho: String;

  procedure Logar(const AString: String);
  begin
    lstMsgInstalacao.Items.Add(AString);
    lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
    Application.ProcessMessages;

    WriteToTXT(PathArquivoLog, AString);
  end;

  procedure MostrarMensagemInstalado(const aMensagem: String; const aErro: String = '');
  var
    Msg: String;
  begin

    Msg := Format(aMensagem + ' em "%s"', [sPathBin]);

    WriteToTXT(PathArquivoLog, '');
    Logar(Msg);
  end;

  procedure IncrementaBarraProgresso;
  begin
    pgbInstalacao.Position := pgbInstalacao.Position + 1;
    Application.ProcessMessages;
  end;

begin

  FCountErros := 0;

  btnInstalar.Enabled := False;
  wizPgInstalacao.EnableButton(bkNext, False);
  wizPgInstalacao.EnableButton(bkBack, False);
  wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), False);
  try
    Cabecalho := 'Caminho: ' + edtDirDestino.Text + sLineBreak +
                 'Vers�o do delphi: ' + edtDelphiVersion.Text + ' (' + IntToStr(iVersion)+ ')' + sLineBreak +
                 'Plataforma: ' + edtPlatform.Text + '(' + IntToStr(Integer(tPlatform)) + ')' + sLineBreak +
                 StringOfChar('=', 80);

    // limpar o log
    lstMsgInstalacao.Clear;
    WriteToTXT(PathArquivoLog, Cabecalho, False);

    // setar barra de progresso
    pgbInstalacao.Position := 0;
    pgbInstalacao.Max := (frameDpk.Pacotes.Count * 2) + 6;


    // *************************************************************************
    // removendo arquivos antigos se configurado
    // *************************************************************************
    (*if ckbRemoverArquivosAntigos.Checked then
    begin
      if Application.MessageBox(
        'voc� optou por limpar arquivos antigos do UserControl do seu computador, essa a��o pode demorar v�rios minutos, deseja realmente continuar com est� a��o?',
        'Limpar',
        MB_YESNO + MB_DEFBUTTON2
      ) = ID_YES then
      begin
        Logar('Removendo arquivos antigos do disco...');
        RemoverArquivosAntigosDoDisco;
        IncrementaBarraProgresso;
      end;
    end; *)


    // *************************************************************************
    // Seta a plataforna selecionada
    // *************************************************************************
    Logar('Setando par�metros de plataforma...');
    SetPlatformSelected;
    IncrementaBarraProgresso;


    // *************************************************************************
    // Cria diret�rio de biblioteca da vers�o do delphi selecionada,
    // s� ser� criado se n�o existir
    // *************************************************************************
    Logar('Criando diret�rios de bibliotecas...');
    CreateDirectoryLibrarysNotExist;
    IncrementaBarraProgresso;


    // *************************************************************************
    // remover paths do delphi
    // *************************************************************************
    (*Logar('Removendo diretorios e pacotes antigos instalados...');
    RemoverDiretoriosEPacotesAntigos; *)
    IncrementaBarraProgresso;


    // *************************************************************************
    // Adiciona os paths dos fontes na vers�o do delphi selecionada
    // *************************************************************************
    Logar('Adicionando library paths...');
    AddLibrarySearchPath;
    IncrementaBarraProgresso;


    // *************************************************************************
    // compilar os pacotes primeiramente
    // *************************************************************************
    Logar('');
    Logar('COMPILANDO OS PACOTES...');
    for iDpk := 0 to frameDpk.Pacotes.Count - 1 do
    begin
      NomePacote := frameDpk.Pacotes[iDpk].Caption;

      // Busca diret�rio do pacote
      ExtrairDiretorioPacote(NomePacote);

      if (IsDelphiPackage(NomePacote)) and (frameDpk.Pacotes[iDpk].Checked) then
      begin
        WriteToTXT(PathArquivoLog, '');

        if oUserControl.Installations[iVersion].CompilePackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
          Logar(Format('Pacote "%s" compilado com sucesso.', [NomePacote]))
        else
        begin
          Inc(FCountErros);
          Logar(Format('Erro ao compilar o pacote "%s".', [NomePacote]));

          // parar no primeiro erro para evitar de compilar outros pacotes que
          // precisam do pacote que deu erro
          Break
        end;
      end;

      IncrementaBarraProgresso;
    end;


    // *************************************************************************
    // instalar os pacotes somente se n�o ocorreu erro na compila��o e plataforma for Win32
    // *************************************************************************
    if (edtPlatform.ItemIndex = 0) then
    begin
      if (FCountErros <= 0) then
      begin
        Logar('');
        Logar('INSTALANDO OS PACOTES...');

        for iDpk := 0 to frameDpk.Pacotes.Count - 1 do
        begin
          NomePacote := frameDpk.Pacotes[iDpk].Caption;

          // Busca diret�rio do pacote
          ExtrairDiretorioPacote(NomePacote);

          if IsDelphiPackage(NomePacote) then
          begin
            // instalar somente os pacotes de designtime
            GetDPKFileInfo(sDirPackage + NomePacote, bRunOnly);
            if not bRunOnly then
            begin
              // se o pacote estiver marcado instalar, sen�o desinstalar
              if frameDpk.Pacotes[iDpk].Checked then
              begin
                WriteToTXT(PathArquivoLog, '');

                if oUserControl.Installations[iVersion].InstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
                  Logar(Format('Pacote "%s" instalado com sucesso.', [NomePacote]))
                else
                begin
                  Inc(FCountErros);
                  Logar(Format('Ocorreu um erro ao instalar o pacote "%s".', [NomePacote]));

                  Break;
                end;
              end
              else
              begin
                WriteToTXT(PathArquivoLog, '');

                if oUserControl.Installations[iVersion].UninstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
                  Logar(Format('Pacote "%s" removido com sucesso...', [NomePacote]));
              end;
            end;
          end;

          IncrementaBarraProgresso;
        end;
      end
      else
      begin
        Logar('');
        Logar('Abortando... Ocorreram erros na compila��o dos pacotes.');
      end;
    end
    else
    begin
      Logar('');
      Logar('Para a plataforma de 64 bits os pacotes s�o somente compilados.');
    end;


    // *************************************************************************
    // n�o instalar outros requisitos se ocorreu erro anteriormente
    // *************************************************************************
    if FCountErros <= 0 then
    begin
      Logar('');
      Logar('INSTALANDO OUTROS REQUISITOS...');

      // *************************************************************************
      // deixar somente a pasta lib se for configurado assim
      // *************************************************************************
      if chkDeixarSomenteLIB.Checked then
      begin
        try
          DeixarSomenteLib;

          MostrarMensagemInstalado('Limpeza library path com sucesso');
          MostrarMensagemInstalado('Copia dos arquivos necess�rio.');
        except
          on E: Exception do
          begin
            MostrarMensagemInstalado('Ocorreu erro ao limpas os path e copiar arquivos' + sLineBreak +E.Message )
          end;
        end;
      end;
    end;
  finally
    btnInstalar.Enabled := True;
    wizPgInstalacao.EnableButton(bkBack, True);
    wizPgInstalacao.EnableButton(bkNext, FCountErros = 0);
    wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), True);
  end;

  if FCountErros = 0 then
  begin
    Application.MessageBox(
      PWideChar(
        'Pacotes compilados e instalados com sucesso! '+sLineBreak+
        'Clique em "Pr�ximo" para finalizar a instala��o.'
      ),
      'Instala��o',
      MB_ICONINFORMATION + MB_OK
    );
  end
  else
  begin
    if Application.MessageBox(
      PWideChar(
        'Ocorreram erros durante o processo de instala��o, '+sLineBreak+
        'para maiores informa��es verifique o arquivo de log gerado.'+sLineBreak+sLineBreak+
        'Deseja visualizar o arquivo de log gerado?'
      ),
      'Instala��o',
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
  // -- Plataforma s� habilita para Delphi XE2
  // -- Desabilita para vers�o diferente de Delphi XE2
  edtPlatform.Enabled := oUserControl.Installations[iVersion].VersionNumber >= 9;
  if oUserControl.Installations[iVersion].VersionNumber < 9 then
    edtPlatform.ItemIndex := 0;

  // C++ Builder a partir do D2006, vers�es anteriores tem IDE independentes.
  ckbBCB.Enabled := MatchText(oUserControl.Installations[iVersion].VersionNumberStr,
    ['d10', 'd11', 'd12', 'd14', 'd15', 'd16', 'd17', 'd18', 'd19', 'd20',
    'd21', 'd22', 'd23', 'd24', 'd25']);
  if not ckbBCB.Enabled then
    ckbBCB.Checked := False;
end;

procedure TFrmPrincipal.ExtrairDiretorioPacote(NomePacote: string);
  procedure FindDirPackage(sDir, sPacote: String);
  var
    oDirList: TSearchRec;
    iRet: Integer;
    sDirDpk: string;
  begin
    sDir := IncludeTrailingPathDelimiter(sDir);
    if not DirectoryExists(sDir) then
      Exit;

    if System.SysUtils.FindFirst(sDir + '*.*', faAnyFile, oDirList) = 0 then
    begin
      try
        repeat

          if (oDirList.Name = '.')  or (oDirList.Name = '..') or (oDirList.Name = '__history') then
            Continue;

          //if oDirList.Attr = faDirectory then
          if DirectoryExists(sDir + oDirList.Name) then
            FindDirPackage(sDir + oDirList.Name, sPacote)
          else
          begin
            if oDirList.Name = sPacote then
              sDirPackage := IncludeTrailingPathDelimiter(sDir);
          end;

        until System.SysUtils.FindNext(oDirList) <> 0;
      finally
        System.SysUtils.FindClose(oDirList);
      end;
    end;
  end;

begin
  sDirPackage := '';

  FindDirPackage(sDirRoot + 'Packages\', NomePacote);
end;

procedure TFrmPrincipal.FindDirs(ADirRoot: String; bAdicionar: Boolean);
var
  oDirList: TSearchRec;

  function EProibido(const ADir: String): Boolean;
  const
    LISTA_PROIBIDOS: ARRAY[0..4] OF STRING = (
      'quick', 'rave', 'laz', 'VerificarNecessidade', '__history'
    );
  var
    Str: String;
  begin
    Result := False;
    for str in LISTA_PROIBIDOS do
    begin
      Result := Pos(AnsiUpperCase(str), AnsiUpperCase(ADir)) > 0;
      if Result then
        Break;
    end;
  end;

begin
  ADirRoot := IncludeTrailingPathDelimiter(ADirRoot);

  if FindFirst(ADirRoot + '*.*', faDirectory, oDirList) = 0 then
  begin
     try
       repeat
          if ((oDirList.Attr and faDirectory) <> 0) and
              (oDirList.Name <> '.')                and
              (oDirList.Name <> '..')               and
              (not EProibido(oDirList.Name)) then
          begin
             with oUserControl.Installations[iVersion] do
             begin
               if bAdicionar then
               begin
                  AddToLibrarySearchPath(ADirRoot + oDirList.Name, tPlatform);
                  AddToLibraryBrowsingPath(ADirRoot + oDirList.Name, tPlatform);
               end
               else
                  RemoveFromLibrarySearchPath(ADirRoot + oDirList.Name, tPlatform);
             end;
             //-- Procura subpastas
             FindDirs(ADirRoot + oDirList.Name, bAdicionar);
          end;
       until FindNext(oDirList) <> 0;
     finally
       System.SysUtils.FindClose(oDirList)
     end;
  end;
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

  // popular o combobox de vers�es do delphi instaladas na m�quina
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
      edtDelphiVersion.Items.Add('Delphi 10 Seattle')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd24' then
      edtDelphiVersion.Items.Add('Delphi 10.1 Berlin')
    else if oUserControl.Installations[iFor].VersionNumberStr = 'd25' then
      edtDelphiVersion.Items.Add('Delphi 10.2 Tokyo');
    // -- Evento disparado antes de iniciar a execu��o do processo.
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

// verificar se no caminho informado j� existe o .svn indicando que o
// checkout j� foi feito no diretorio
function TfrmPrincipal.IsCheckOutJaFeito(const ADiretorio: String): Boolean;
begin
  Result := DirectoryExists(IncludeTrailingPathDelimiter(ADiretorio) + '.svn')
end;

procedure TFrmPrincipal.wizPgConfiguracaoCancelButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  if Application.MessageBox(
    'Deseja realmente cancelar a instala��o?',
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
  // verificar se foi informado o diret�rio
  if Trim(edtDirDestino.Text) = EmptyStr then
  begin
    Stop := True;
    edtDirDestino.SetFocus;
    Application.MessageBox(
      'Diret�rio de instala��o n�o foi informado.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  // prevenir vers�o do delphi em branco
  if Trim(edtDelphiVersion.Text) = '' then
  begin
    Stop := True;
    edtDelphiVersion.SetFocus;
    Application.MessageBox(
      'Vers�o do delphi n�o foi informada.',
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
      'Plataforma de compila��o n�o foi informada.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  // Gravar as configura��es em um .ini para utilizar depois
  GravarConfiguracoes;

  //if not DirectoryExists() then

end;

procedure TFrmPrincipal.wizPgInstalacaoEnterPage(Sender: TObject;
  const FromPage: TJvWizardCustomPage);
begin
  SetPlatformSelected;
  lstMsgInstalacao.Clear;
  pgbInstalacao.Position := 0;

  // mostrar ao usu�rio as informa��es de compila��o
  with lbInfo.Items do
  begin
    Clear;
    Add(edtDelphiVersion.Text + ' ' + edtPlatform.Text);
    Add('Dir. Instala��o  : ' + edtDirDestino.Text);
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
      'Clique no bot�o instalar antes de continuar.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  if (FCountErros > 0) then
  begin
    Stop := True;
    Application.MessageBox(
      'Ocorreram erros durante a compila��o e instala��o dos pacotes, verifique.',
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

// Evento disparado a cada a��o do instalador
procedure TfrmPrincipal.OutputCallLine(const Text: string);
begin
  // remover a warnings de convers�o de string (delphi 2010 em diante)
  // as diretivas -W e -H n�o removem estas mensagens
  if (pos('Warning: W1057', Text) <= 0) and ((pos('Warning: W1058', Text) <= 0)) then
    WriteToTXT(PathArquivoLog, Text);
end;

// retornar o path do aplicativo
function TfrmPrincipal.PathApp: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

// retornar o caminho completo para o arquivo .ini de configura��es
function TfrmPrincipal.PathArquivoIni: String;
var
  NomeApp: String;
begin
  NomeApp := ExtractFileName(ParamStr(0));
  Result := PathApp + ChangeFileExt(NomeApp, '.ini');
end;

// retornar o caminho completo para o arquivo de logs
function TfrmPrincipal.PathArquivoLog: String;
begin
  Result := PathApp + 'log_' +
  StringReplace(edtDelphiVersion.Text, ' ', '_', [rfReplaceAll]) + '.txt';
end;

function TFrmPrincipal.RunAsAdminAndWaitForCompletion(hWnd: HWND;
  filename: string): Boolean;
{
    See Step 3: Redesign for UAC Compatibility (UAC)
    http://msdn.microsoft.com/en-us/library/bb756922.aspx
}
var
  sei: TShellExecuteInfo;
  ExitCode: DWORD;
begin
  ZeroMemory(@sei, SizeOf(sei));
  sei.cbSize       := SizeOf(TShellExecuteInfo);
  sei.Wnd          := hwnd;
  sei.fMask        := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI or SEE_MASK_NOCLOSEPROCESS;
  sei.lpVerb       := PWideChar('runas');
  sei.lpFile       := PWideChar(Filename);
  sei.lpParameters := PWideChar('');
  sei.nShow        := SW_HIDE;

  if ShellExecuteEx(@sei) then
  begin
    repeat
      Application.ProcessMessages;
      GetExitCodeProcess(sei.hProcess, ExitCode) ;
    until (ExitCode <> STILL_ACTIVE) or  Application.Terminated;
  end;
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
  else
  if edtPlatform.ItemIndex = 1 then // Win64
  begin
    tPlatform := bpWin64;
    sDirLibrary := sDirRoot + sTipo + 'Lib' + sVersao + 'x64';
  end;

  { Cria os diretorios }
  if not (DirectoryExists(sDirRoot + sTipo)) then
    MkDir(sDirRoot + sTipo);

  if not (DirectoryExists(sDirLibrary)) then
    MkDir(sDirLibrary);
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
