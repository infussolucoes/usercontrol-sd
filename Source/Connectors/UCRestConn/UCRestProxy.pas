{ -----------------------------------------------------------------------------
  Unit Name: UCDataSnapConn
  Author   : Giovani Da Cruz
  Date     : 26/12/2015
  Purpose  : DataSnap Suporte
  E-mail   : giovani@infus.inf.br
  URL      : www.infus.inf.br
  UC SW    : http://infussolucoes.github.io/usercontrol-sd/

  registered in UCDataSnapConnReg.pas
  ----------------------------------------------------------------------------- }

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
{ **************************************************************************** }

{ ******************************************************************************
  |* Historico
  |*
  |* 26/12/2015: Giovani Da Cruz
  |*  - Criação e distribuição da Primeira deste connector
  |*
  **************************************************************************** }

(* Nota do autor:

    Você precisa criar métodos correspondentes no seu servidor e quando gerar
    as classes por proxi, ao menos uma deverá herdar de TDSUserRemote.
    A classe deverá estar com os métodos abstratos implementados.

*)

unit UCRestProxy;

interface

uses Datasnap.DSProxyRest, Data.FireDACJSONReflect;

type
  TDSUserRemote = class(TDSAdminRestClient)
  public
    { Recupera um Dataset para um TFDMemTable }
    function GetDataSet2(SQL: string; const ARequestFilter: string = ''): TFDJSONDataSets; virtual; abstract;

    { Executa uma instrução SQL nativa direto no banco de dados }
    procedure ExecuteSQL(SQL: string); virtual; abstract;

    { Verifica se a tabela está presente no banco de dados }
    function FindTable(TableName: string; SchemaName: string; const ARequestFilter: string = ''): Boolean; virtual; abstract;
  end;

implementation


end.
