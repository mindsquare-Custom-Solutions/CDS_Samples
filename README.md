<img src="https://mindsquare.de/files/logo-mindsquare-176x781.png" alt="mindsquare Logo" title="mindsquare AG" align="right">

# Begleitmaterialien zur Core Data Services (CDS) Schulung

Codebeispiele für die [mindsquare Core Data Services Schulung](https://mindsquare.de/schulungen/)

## Datenmodell

TODO

## Core Data Services - Grundlagen

### Syntax DDIC-based CDS View

```cds
TODO
```

### Syntax CDS View Entity

>Ab S/4HANA 2020

```cds
TODO
```

### Cast Operator

```cds
define view entity ZI_CAST 
  as data source from TABLE
  {
    key key_field               as KeyField,
        cast(<field> as <type>) as FieldName
  }
```

### Literale

> Typisierte Literale ab S/4HANA 2020 und nur in CDS View Entities unterstützt

```cds
define view entity ZMIND2EG_I_Literals
  as select from zmind2_airport
  {
    // Untypisierte Literale
    '123'               as TypeNumc,
    'ABC123'            as TypeChar,
    123                 as TypeInt,
    12.34               as TypeFltp,

    // Typisierte Literale
    abap.datn'20200101' as TypeDatn,
    abap.string'Text'   as TypeString
  }
```

### Case Operator

#### Einfach Fallunterscheidung

```cds
define view entity ZI_CASE 
  as data source from TABLE
{
  key keyField,
      case name
        when 'Anna' then 'X'
        when 'Lisa' then 'Y'
        else '-'
      end as first_name
}
```

#### Komplexe Fallunterscheidung

```cds
define view entity ZI_CASE 
  as data source from TABLE
{
  key keyField,
      case
        when distance >= 2000 
          then 'long-haul flight'
        when distance >= 1000 and distance <  2000 
          then 'medium-haul flight'
        when distance <  1000 
          then 'short-haul flight'
        else 'error'
      end as flight_type
}
```

### Session Variablen

```cds
define view entity demo_cds_session_variables_ve
  as select from demo_expressions
{
  key id,
      $session.user            as system_user,
      $session.client          as system_client,
      $session.system_language as system_language,
      $session.system_date     as system_date,
      $session.user_timezone   as user_timezone,
      $session.user_date       as user_date
}
```

### Build-In Functions

[SAP Help Dokumentation](https://help.sap.com/doc/abapdocu_latest_index_htm/latest/en-US/index.htm?file=abencds_builtin_functions_v2.htm)

### Wiederverwendung von Feldern

>Ab S/4HANA 2022

```cds
define view entity DEMO_CDS_EXPRESSION_REUSE
  as select from demo_expressions
{
  key id as field1,
      concat($projection.field1, 'x') as field_reuse,

      //literal
      abap.char'hallo‘ as lit1,
      concat($projection.lit1, 'x')   as lit_reuse,

      //arithmetic expression
      abap.decfloat34'123.45E6‘ as arith,
      $projection.arith * 2 as arith_reuse,

      //cast expression
      cast(char1 as abap.numc(10)) as cast1,
      coalesce($projection.cast1, numc2)as cast_reuse
}
```

### Filterung

```cds
define view entity DEMO_SALES_ORDER_WHERE
  as select from demo_sales_order
  association to demo_sales_bupa as _partner on $projection.id = _partner.id
{
  key so_key,
      id,
      company_code,
      _partner.family_name
}
where
      _partner.family_name like 'S%'
  and length( _partner.family_name ) = abap.int1'4'
  and created_on between abap.dats'20200101' and abap.dats'20200401'
  and cast( _partner.birth_name as demo_sales_birth_name preserving type ) = abap.char'Meier'
```

### Aggregation

```cds
define view entity DEMO_CDS_VIEW_ENTITY_HAVING
  as select from sflight
{
  concat_with_space(carrid, connid, 1) as ID,
  count(*)                             as col_count,
  seatsmax
}
group by
  carrid,
  connid,
  seatsmax
having
  seatsmax > abap.int2'500'
```

### Parameter

```cds
TODO
```

```cds
TODO Select einer CDS View mit Parameter
```

## CDS in ABAP

### OpenSQL Select

```abap
TODO zmind2_select
```

```cds
TODO
```

#### OpenSQL Select mit Parameter

```abap
TODO
```

#### OpenSQL Select mit Assoziation

```abap
TODO
```

### ALV Integrated Data Access

```abap
TODO zmind2_alv_ida
```

#### ALV IDA mit Parameter

```cds
TODO
```

## Annoatationen

### Annotationen Syntax

```cds
TODO ddic-based view
```

```cds
TODO CDS View Entity
```

### Bezeichnertexte

```cds
TODO
```

### Einheitenreferenzen

```cds
TODO
```

### Währungsreferenzen

```cds
TODO
```

### Kontrakt für Key-User-Felderweiterung 

```cds
TODO
```

### Deprecation

```cds
TODO
```

### Access Control Annotation

```cds
TODO
```

### Fremdschlüsselbeziehung

CDS View mit Fremdschlüssel:

```cds
TODO
```

CDS View mit repräsentativem Schlüssel:

```cds
TODO
```

### Textbeziehungen

#### Textbeziehung innerhalb einer View

```cds
TODO
```

#### Textbeziehung mit Text View

```cds
TODO
```

```cds
TODO
```

### Metadatenerweiterungen

Erweiterte CDS View Entity:

```cds
TODO
```

Metadatenwerweiterung:

```cds
TODO
```

## CDS View Erweiterung

```cds
TODO
```

## Entity Buffer

### Entity Buffer Annotation

```cds
TODO
```

### Entity Buffer Syntax

```cds
TODO
```

## Komposition

### Join

```cds
TODO
```

### Assoziationen

#### Syntax

```cds
TODO
```

#### Komplexe Komposition

```cds
TODO
```

### Union

```cds
TODO
```

## Access Control

### Access Control Syntax

```cds
TODO
```

### Access Control Vererbung

```cds
TODO
```

### Access Control Redefinition

```cds
TODO
```

### Access Control Umbenennung

```cds
TODO
```

### Access Control Optionale Elemente

```cds
TODO
```

### Access Control Assoziationen

```cds
TODO
```

### Access Control Parameter

```cds
TODO
```

### Access Control Anpassung von Standardzugriffskontrollen

Combination Mode or:

```cds
TODO
```

Combination Mode and:

```cds
TODO
```

Redefinition:

```cds
TODO
```

```cds
TODO
```

### Access Control Nutzername

```cds
TODO
```

### Weitere Arten von Zugriffskontrollen

```cds
TODO
```

```cds
TODO
```

```cds
TODO
```

```cds
TODO
```

## Service Definition

```cds
TODO
```

## Fiori Elements

### Line Item

```cds
TODO
```

### Freitextsuche

```cds
TODO
```

### Filter

```cds
TODO
```

### Suchhilfen

```cds
TODO
```

### Tabellenbezeichnung

```cds
TODO
```

### Semantische Hervorhebung

```cds
TODO
```

### Gruppieren & Sortieren

```cds
TODO
```

### Seitentitel

```cds
TODO
```

### Facetten

```cds
TODO
```

## Virtuelle Elemente

### Annotationen für virtuelle Elemente

```cds
TODO
```

### Virtuelle Elemente: Datenberechnung

```abap
TODO
```

### Virtuelle Elemente: Filterung

```abap
TODO
```

### Virtuelle Elemente: Sortierung

```abap
TODO
```

## Analytics

### Cube View

```cds
TODO
```

### Dimension View

```cds
TODO
```

### Analytische Query

```cds
TODO
```

## Hierarchien

### Analytische Hierarchien

```cds
TODO
```

### CDS Hierarchien

```cds
TODO
```

```cds
TODO
```

## Code Pushdown

### AMDP

```abap
TODO
```

### Table Function

```cds
TODO
```

#### AMDP für Table Function

```abap
TODO
```

#### Konsumierung von Table Function

```cds
TODO
```

## Test Double Framework

### Testklasse

Globale Klasse:

```abap
TODO
```

Lokale Testklasse:

```abap
TODO
```

#### Testdaten

```abap
TODO
```

#### Testmethode

```abap
TODO
```

#### Prüfen von Assoziationen

```abap
TODO
```

#### Prüfen von Joins

```abap
TODO
```

#### Unit Test für mehrere Entitäten

```abap
TODO
```

### Annotationen auslesen

```abap
TODO
```

### Test Double mit Parameter

```abap
TODO
```

### Test Double mit Konvertierungsfunktionen

```abap
TODO
```

### Test Relations

```abap
TODO
```

### Berechtigungen testen

#### Berechtigungsprüfung deaktivieren

```abap
TODO
```

#### Test unberechtigter Nutzer

```abap
TODO
```

#### Test: Berechtigungen mocken

```abap
TODO
```