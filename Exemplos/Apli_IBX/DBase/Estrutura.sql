/******************************************************************************/
/****       User Control Show Delphi Edition - www.showdelphi.com.br       ****/
/******************************************************************************/

SET SQL DIALECT 3;


/******************************************************************************/
/****                                Tables                                ****/
/******************************************************************************/

CREATE TABLE BANCOS (
    CODIGO_B        INTEGER NOT NULL,
    NOME            CHAR(30),
    CREDITO_DISPON  FLOAT,
    LIMITE          FLOAT
);


CREATE TABLE CIDADES (
    CODIGO      INTEGER NOT NULL,
    NOMECIDADE  VARCHAR(30),
    CEP         CHAR(10),
    UF          CHAR(2)
);


CREATE TABLE CLIENTES (
    CODIGO    INTEGER NOT NULL,
    NOME      VARCHAR(30),
    CNPJ_CPF  VARCHAR(20),
    IDADE     SMALLINT
);


CREATE TABLE PRODUTOS (
    CODIGO_P      INTEGER NOT NULL,
    DESCRICAO_P   VARCHAR(30),
    UNIDADE_P     VARCHAR(3),
    VALOR_UNIT_P  FLOAT
);





/******************************************************************************/
/****                             Primary Keys                             ****/
/******************************************************************************/

ALTER TABLE BANCOS ADD CONSTRAINT PK_BANCOS PRIMARY KEY (CODIGO_B);
ALTER TABLE CIDADES ADD CONSTRAINT PK_CIDADES PRIMARY KEY (CODIGO);
ALTER TABLE CLIENTES ADD CONSTRAINT PK_CLIENTES PRIMARY KEY (CODIGO);
ALTER TABLE PRODUTOS ADD CONSTRAINT PK_PRODUTOS PRIMARY KEY (CODIGO_P);
