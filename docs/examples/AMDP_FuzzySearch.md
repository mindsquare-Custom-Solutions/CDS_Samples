# AMDP Fuzzy Search

Beispielhafte Implementierung einer Fuzzy Search auf einer Datenbanktabelle mit den HANA DB Suchfunktionen über SQL. Die Implementierung erfolgt mithilfe von AMDP und Table Functions. Alle Möglichkeiten der Suche mit der HANA DB sind in der [Dokumentation](https://help.sap.com/docs/SAP_HANA_PLATFORM/691cb949c1034198800afde3e5be6570/cd07da82bb571014b185c8e3e3974767.html) beschrieben.

Die Beispielimplementierung sucht in der Spalte `Description` der Datenbanktabelle `zmind2_travel`. 

## Table Function

Anlegen als `Data Definition` mit dem Template `Define Table Function with Parameter`.

**ZI_MIND2_FuzzySearchTF**
```cds
@EndUserText.label: 'Fuzzy Search by Table Function Demo'
define table function ZI_MIND2_FuzzySearchTF
  with parameters
    @Environment.systemField: #CLIENT
    client : abap.clnt,
    search_key : abap.char( 80 )
returns
{
  key Client      : abap.clnt;
  key TravelId    : zmind2_travel_id;
      Score       : abap.dec( 3, 2 );
      Description : zmind2_description;
}
implemented by method
  zcl_mind2_amdp_fuzzysearchtf=>fuzzy_search;
```

## AMDP

Anlegen einer ABAP Klasse mit dem Interface `if_amdp_marker_hdb`. Implementierung der Methode erfolgt in SQL Script.

**ZCL_MIND2_AMDP_FUZZYSEARCHTF**
```abap
CLASS zcl_mind2_amdp_fuzzysearchtf DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS fuzzy_search FOR TABLE FUNCTION ZI_MIND2_FuzzySearchTF.


    INTERFACES if_amdp_marker_hdb .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_mind2_amdp_fuzzysearchtf IMPLEMENTATION.
  METHOD fuzzy_search BY DATABASE FUNCTION FOR HDB LANGUAGE SQLSCRIPT OPTIONS READ-ONLY USING zmind2_travel.
    RETURN select client, travel_id as TravelId, score( ) as Score, description
        from zmind2_travel as travel
        where client = :client and contains( travel.description, :search_key, FUZZY(0.8, 'spellCheckFactor=1.0') )
        order by Score desc;
  endmethod.
ENDCLASS.
```

