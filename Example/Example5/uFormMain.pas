{-----------------------------------------------------------------------------
   Example5
   Copyright (C) 2024-2030 Muzio Valerio

   Unit Name: uFormMain
   Author:    muzio
   Date:      11-feb-2024

   Info:
   Purpose:
   History:

-----------------------------------------------------------------------------}

unit uFormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.Actions, Vcl.ActnList,
  XML.Writer.Types;

type
  TFormMain = class(TForm)
    TopPanel: TPanel;
    LabelPath: TLabel;
    ExecuteButton: TButton;
    ExitButton: TButton;
    EditPath: TEdit;
    ActionList: TActionList;
    ExecuteAction: TAction;
    ExitAction: TAction;
    mXMLView: TMemo;
    cbIncludeHedaer: TCheckBox;
    EditEncoding: TEdit;
    cbVersione: TComboBox;
    rgTextEncoder: TRadioGroup;
    procedure ExecuteActionExecute(Sender: TObject);
    procedure ExitActionExecute(Sender: TObject);
  private
    procedure FatturaElettronica(var aNodeList: TXMLNodeList);
    // Header
    procedure FatturaHeader(var aNodeList: TXMLNodeList);
    procedure DatiTrasmissione(const aNodeList: TXMLNodeList);
    procedure IdTrasmittente(const aNodeList: TXMLNodeList);

    // Cedente Prestatore
    procedure CedentePrestatore(const aNodeList: TXMLNodeList);
    procedure DatiAnagraficiCP(const aNodeList: TXMLNodeList);
    procedure AnagraficaCP(const aNodeList: TXMLNodeList);
    procedure IdFiscaleIVACP(const aNodeList: TXMLNodeList);
    procedure SedeCP(const aNodeList: TXMLNodeList);

    // Cessionario Committente
    procedure CessionarioCommittente(const aNodeList: TXMLNodeList);
    procedure DatiAnagraficiCC(const aNodeList: TXMLNodeList);
    procedure AnagraficaCC(const aNodeList: TXMLNodeList);
    procedure SedeCC(const aNodeList: TXMLNodeList);
    procedure RappresentateFiscaleCC(const aNodeList: TXMLNodeList);
    procedure IdFiscaleIVARF(const aNodeList: TXMLNodeList);

    // Body
    procedure FatturaBody(var aNodeList: TXMLNodeList);
    procedure DatiGenerali(const aNodeList: TXMLNodeList);
    procedure DatiGeneraliDocumento(const aNodeList: TXMLNodeList);
    procedure DatiOrdineAcquisto(const aNodeList: TXMLNodeList);
    procedure DatiTrasporto(const aNodeList: TXMLNodeList);
    procedure DatiAnagraficaVettore(const aNodeList: TXMLNOdeList);
    procedure IdFiscaleIVAVett(const aNodeList: TXMLNodeList);
    procedure AnagraficaVett(const aNodeList: TXMLNodeList);
    procedure DatiBeniServizi(const aNodeList: TXMLNodeList);
    procedure DettaglioLinee(const aNodeList: TXMLNodeList; const aLinea: Integer; const WithScontoMaggiorazione: Boolean =
        False);
    procedure ScontoMaggiorazione(const aNodeList: TXMLNodeList);
    procedure DatiRiepilogo(const aNodeList: TXMLNodeList);
    procedure DatiPagamento(const aNodeList: TXMLNodeList);
    procedure DettaglioPagamento(const aNodeList: TXMLNodeList);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

uses
  XML.Writer;

procedure TFormMain.ExitActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.ExecuteActionExecute(Sender: TObject);
var
  lStringStream: TStringStream;
begin
  var lXMLWriter := TXMLWriter.Create;
  lStringStream := TStringStream.Create;
  try
    lXMLWriter.AutoIndent := True;
    lXMLWriter.IncludeHeader := cbIncludeHedaer.Checked;
    lXMLWriter.Encoding := EditEncoding.Text;

    lXMLWriter.Nodes.Clear;
    var lNodes := lXMLWriter.Nodes;
    FatturaElettronica(lNodes);
    lXMLWriter.SaveToFile(EditPath.Text);
    case rgTextEncoder.ItemIndex of
      0: lXMLWriter.SaveToStream(lStringStream, False);
      1: lXMLWriter.SaveToStream(lStringStream, True);
    end;

    lStringStream.Position := 0;
    mXMLView.Lines.LoadFromStream(lStringStream);
  finally
    FreeAndNil(lStringstream);
    FreeAndNil(lXMLWriter);
  end;
end;

procedure TFormMain.FatturaElettronica(var aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('p:FatturaElettronica').
  AddAttribute('versione', cbVersione.Items[cbVersione.ItemIndex]).
  AddAttribute('SistemaEmittente', 'abc').
  AddAttribute('xmlns:ds', 'http://www.w3.org/2000/09/xmldsig#').
  AddAttribute('xmlns:p', 'http://ivaservizi.agenziaentrate.gov.it/docs/xsd/fatture/v1.2').
  AddAttribute('xmlns:xsi', 'http://www.w3.org/2001/XMLSchema-instance').
  AddAttribute('xsi:schemaLocation', 'http://ivaservizi.agenziaentrate.gov.it/docs/xsd/fatture/v1.2 fatturaordinaria_v1.2.xsd');
  FatturaHeader(aNodeList);
  FatturaBody(aNodeList);
  aNodeList.CloseNode;
end;

procedure TFormMain.FatturaHeader(var aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('FatturaEletronicaHeader').
    AddAttribute('Attribute', 'FirstAttribute').
    AddAttribute('MyAttribute', 'SecondAttribute');
  DatiTrasmissione(aNodeList);
  CedentePrestatore(aNodeList);
  CessionarioCommittente(aNodeList);
  aNodeList.CloseNode;
end;

procedure TFormMain.DatiTrasmissione(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('DatiTrasmissione').AddAttribute('Dato', 'Trasmissione');
  IdTrasmittente(aNodeList);
  aNodeList.AddLeafNode('ProgressivoInvio').AsString('00001').AddAttribute('Attributo', 'Progressivo');
  aNodeList.AddLeafNode('FormatoTrasmissione').AsString('FPR12');
  aNodeList.AddLeafNode('CodiceDestinatario').AsString('0000000');
  aNodeList.AddLeafNode('ContattiTrasmittente');
  aNodeList.AddLeafNode('PECDestinatario').AsString('betagamma@pec.it');
  aNodeList.CloseNode;
end;

procedure TFormMain.IdTrasmittente(const aNodeList: TXMLNodeList);
begin
  with aNodeList do
  begin
    OpenNode('IdTrasmittente');
    AddLeafNode('IdPaese').AsString('IT');
    AddLeafNode('IdCodice').AsString('01234567890');
    CloseNode;
  end;
end;

procedure TFormMain.CedentePrestatore(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('CedentePrestatore');
  DatiAnagraficiCP(aNodeList);
  SedeCP(aNodeList);
  aNodeList.CloseNode;
end;

procedure TFormMain.DatiAnagraficiCP(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('DatiAnagrafici');
  IdFiscaleIVACP(aNodeList);
  AnagraficaCP(aNodeList);
  aNodeList.AddLeafNode('RegimeFiscale').AsString('RF01');
  aNodeList.CloseNode;
end;

procedure TFormMain.IdFiscaleIVACP(const aNodeList: TXMLNodeList);
begin
  with aNodeList do
  begin
    OpenNode('IdFiscaleIVA');
    AddLeafNode('IdPaese').AsString('IT');
    AddLeafNode('IdCodice').AsString('01234567890');
    CloseNode;
  end;
end;

procedure TFormMain.AnagraficaCP(const aNodeList: TXMLNodeList);
begin
  with aNodeList do
  begin
    OpenNode('Anagrafica');
    AddLeafNode('Denominazione').AsString('SOCIETA'' ALFA SRL');
    AddLeafNode('Titolo');
    CloseNode;
  end;
end;

procedure TFormMain.SedeCP(const aNodeList: TXMLNodeList);
begin
  with aNodeList do
  begin
    OpenNode('SedeCP');
    AddLeafNode('Indirizzo').AsString('VIALE ROMA 543');
    AddLeafNode('CAP').AsString('07100');
    AddLeafNode('Comune').AsString('SASSARI');
    AddLeafNode('Provincia').AsString('SS');
    AddLeafNode('Nazione').AsString('IT');
    CloseNode;
  end;
end;

procedure TFormMain.CessionarioCommittente(const aNodeList: TXMLNodeList);
begin
  with aNodeList do
  begin
    OpenNode('CessionarioCommittente');
    DatiAnagraficiCC(aNodeList);
    SedeCC(aNodeList);
    RappresentateFiscaleCC(aNodeList);
    CloseNode;
  end;
end;

procedure TFormMain.DatiAnagraficiCC(const aNodeList: TXMLNodeList);
begin
  with aNodeList do
  begin
    OpenNode('DatiAnagrafici');
    AddLeafNode('CodiceFiscale').AsString('09876543210');
    AnagraficaCC(aNodeList);
    CloseNode;
  end;
end;

procedure TFormMain.AnagraficaCC(const aNodeList: TXMLNodeList);
begin
  with aNodeList do
  begin
    OpenNode('Anagrafica');
    AddLeafNode('Denominazione').AsString('BETA GAMMA');
    AddLeafNode('Titolo');
    CloseNode;
  end;
end;

procedure TFormMain.SedeCC(const aNodeList: TXMLNodeList);
begin
  with aNodeList do
  begin
    OpenNode('SedeCP');
    AddLeafNode('Indirizzo').AsString('VIA TORINO 38-B');
    AddLeafNode('CAP').AsString('00145');
    AddLeafNode('Comune').AsString('ROMA');
    AddLeafNode('Provincia').AsString('RM');
    AddLeafNode('Nazione').AsString('IT');
    CloseNode;
  end;
end;

procedure TFormMain.RappresentateFiscaleCC(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('RappresentateFiscale');
  IdFiscaleIVARF(aNodeList);
  aNodeList.AddLeafNode('Denominazione').AsString('Rappresentante Fiscale');
  aNodeList.CloseNode;
end;

procedure TFormMain.IdFiscaleIVARF(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('IdFiscaleIVA');
  aNodeList.AddLeafNode('IdPaese').AsString('IT');
  aNodeList.AddLeafNode('IdCodice').AsString('01234567891');
  aNodeList.CloseNode;
end;

procedure TFormMain.FatturaBody(var aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('FatturaElettronicaBody');
  DatiGenerali(aNodeList);
  DatiBeniServizi(aNodeList);
  DatiPagamento(aNodeList);
  aNodeList.CloseNode;
end;

procedure TFormMain.DatiGenerali(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('DatiGenerali');
  DatiGeneraliDocumento(aNodeList);
  DatiOrdineAcquisto(aNodeList);
  DatiTrasporto(aNodeList);
  aNodeList.CloseNode;
end;

procedure TFormMain.DatiGeneraliDocumento(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('DatiGeneraliDocumento');
  aNodeList.AddLeafNode('TipoDocumento').AsString('TD01');
  aNodeList.AddLeafNode('Divisa').AsString('EUR');
  aNodeList.AddLeafNode('Data').AsDate(StrToDate('18/12/2014'));
  aNodeList.AddLeafNode('Numero').AsString('123');
  aNodeList.AddLeafNode('Causale').AsString('LA FATTURA FA RIFERIMENTO AD UNA OPERAZIONE AAAA BBBBBBBBBBBBBBBBBB CCC DDDDDDDDDDDDDDD E FFFFFFFFFFFFFFFFFFFF GGGGGGGGGG HHHHHHH II LLLLLLLLLLLLLLLLL MMM NNNNN OO PPPPPPPPPPP QQQQ RRRR SSSSSSSSSSSSSS');
  aNodeList.AddLeafNode('Causale').AsString('SEGUE DESCRIZIONE CAUSALE NEL CASO IN CUI NON SIANO STATI SUFFICIENTI 200 CARATTERI AAAAAAAAAAA BBBBBBBBBBBBBBBBB');
  aNodeList.CloseNode;
end;

procedure TFormMain.DatiOrdineAcquisto(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('DatiOrdineAcquisto');
  aNodeList.AddLeafNode('RiferimentoNumeroLinea').AsInteger(1).AddAttribute('Attributo', 'Numero').AddAttribute('Secondo', 'Attributo');
  aNodeList.AddLeafNode('IdDocumento').AsString('66685');
  aNodeList.AddLeafNode('NumItem').AsInteger(1);
  aNodeList.CloseNode;
end;

procedure TFormMain.DatiTrasporto(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('DatiTrasporto');
  DatiAnagraficaVettore(aNodeList);
  aNodeList.AddLeafNode('DataOraconsegna').AsDateTime(StrToDateTime('22/10/2014 16:46:12'));
  aNodeList.CloseNode;
end;

procedure TFormMain.DatiAnagraficaVettore(const aNodeList: TXMLNOdeList);
begin
  aNodeList.OpenNode('DatiAnagraficaVettore');
  IdFiscaleIVAVett(aNodeList);
  AnagraficaVett(aNodeList);
  aNodeList.CloseNode;
end;

procedure TFormMain.IdFiscaleIVAVett(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('IdFiscaleIVA');
  aNodeList.AddLeafNode('IdPaese').AsString('IT');
  aNodeList.AddLeafNode('IdCodice').AsString('24681012141');
  aNodeList.CloseNode;
end;

procedure TFormMain.AnagraficaVett(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('Anagrafica');
  aNodeList.AddLeafNode('Denominazione').AsString('Trasporto spa');
  aNodeList.CloseNode;
end;

procedure TFormMain.DatiBeniServizi(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('DatiBeniServizi');
  DettaglioLinee(aNodeList, 1);
  DettaglioLinee(aNodeList, 2);
  DettaglioLinee(aNodeList, 3, True);
  DettaglioLinee(aNodeList, 4, True);
  DatiRiepilogo(aNodeList);
  aNodeList.CloseNode;
end;

procedure TFormMain.DettaglioLinee(const aNodeList: TXMLNodeList; const aLinea: Integer; const WithScontoMaggiorazione:
    Boolean = False);
begin
  aNodeList.OpenNode('DettaglioLinee');
  aNodeList.AddLeafNode('NumeroLinea').AsInteger(aLinea);
  aNodeList.AddLeafNode('Descrizione').AsString('LA DESCRIZIONE DELLA FORNITURA PUO'' SUPERARE I CENTO CARATTERI CHE RAPPRESENTAVANO ' +
    'IL PRECEDENTE LIMITE DIMENSIONALE. TALE LIMITE NELLA NUOVA VERSIONE E'' STATO PORTATO A MILLE CARATTERI');
  aNodeList.AddLeafNode('Quantita').AsFloat(5.00);
  aNodeList.AddLeafNode('UnitaMisura').AsString('Pezzo');
  aNodeList.AddLeafNode('PrezzoUnitario').AsFloat(1.00);
  if WithScontoMaggiorazione then
    ScontoMaggiorazione(aNodeList);
  aNodeList.AddLeafNode('PrezzoTotale').AsFloat(5.00);
  aNodeList.AddLeafNode('AliquotaIVA').AsFloat(22.00);
  aNodeList.CloseNode;
end;

procedure TFormMain.ScontoMaggiorazione(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('ScontoMaggiorazione');
  aNodeList.AddLeafNode('Tipo').AsString('SC');
  aNodeList.AddLeafNode('Importo').AsFloat(-1.71);
  aNodeList.CloseNode;
end;

procedure TFormMain.DatiRiepilogo(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('DatiRiepilogo');
  aNodeList.AddLeafNode('AliquotaIVA').AsFloat(22.00);
  aNodeList.AddLeafNode('ImponibileImporto').AsFloat(36.08);
  aNodeList.AddLeafNode('Imposta').AsFloat(7.94);
  aNodeList.AddLeafNode('EsigibilitaIVA').AsString('D');
  aNodeList.CloseNode;
end;

procedure TFormMain.DatiPagamento(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('DatiPagamento');
  aNodeList.AddLeafNode('CondizioniPagamento').AsString('TP01');
  DettaglioPagamento(aNodeList);
  aNodeList.CloseNode;
end;

procedure TFormMain.DettaglioPagamento(const aNodeList: TXMLNodeList);
begin
  aNodeList.OpenNode('DettaglioPagamento');
  aNodeList.AddLeafNode('ModalitaPagamento').AsString('MP01');
  aNodeList.AddLeafNode('DataScadenzaPagamento').AsDate(StrToDate('30/01/2015'));
  aNodEList.AddLeafNode('ImportoPagamento').AsFloat(36.08);
  aNodeList.CloseNode;
end;

end.
