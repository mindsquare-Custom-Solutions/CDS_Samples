<img src="https://mindsquare.de/files/logo-mindsquare-176x781.png" alt="mindsquare Logo" title="mindsquare AG" align="right">

# Begleitmaterialien zur Core Data Services (CDS) Schulung

Codebeispiele für die [mindsquare Core Data Services Schulung](https://mindsquare.de/schulungen/)

## Datenmodell

TODO

## Core Data Services - Grundlagen

### Syntax DDIC-based CDS View

```cds
@AbapCatalog.sqlViewName: 'ZMS2IAIRPORT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Airport View - CDS Data Model'

@VDM.viewType: #BASIC

@ObjectModel.representativeKey: 'AirportID'

define view ZMIND2_I_AIRPORT
  as select from zmind2_airport as Airport
{
  key Airport.airport_id as AirportID,
      Airport.name       as Name,
      Airport.city       as City,
      Airport.country    as CountryCode
}

```

### Syntax CDS View Entity

>Ab S/4HANA 2020

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Airport Basic View Entity'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@VDM.viewType: #BASIC

@ObjectModel.representativeKey: 'AirportId'

define view entity ZMIND2E_I_Airport
  as select from zmind2_airport
{
  key airport_id as AirportId,
      name       as Name,
      city       as City,
      country    as CountryCode
}
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
define view entity ZIMIND2_EX_Parameter1
  with parameters
    P_Parameter1 :abap.char( 1 ),
    P_Parameter2 :abap.char( 1 )
  as select from zmind2_airport
{
  key airport_id as AirportId,
      $parameters.P_Parameter1 as Parameter1,
      $parameters.P_Parameter2 as Parameter2
}
```

```cds
define view entity ZIMIND2_EX_Parameter2
  with parameters
    P_Parameter1 :abap.char( 1 ),
    P_Parameter2 :abap.char( 1 )
  as select from ZIMIND2_EX_Parameter1(P_Parameter1:$parameters.P_Parameter1, P_Parameter2: 'E')
  association [1..1] to ZIMIND2_EX_Parameter1 as _Association on $projection.AirportId = _Association.AirportId
{
  key AirportId,
      Parameter1,
      Parameter2,

      _Association(P_Parameter1:$parameters.P_Parameter1, P_Parameter2:$parameters.P_Parameter2).Parameter2 as ParameterAssoc,

      _Association
}
```

## CDS in ABAP

### OpenSQL Select

```abap
REPORT zmind2_select.

SELECT FROM zmind2e_c_carrier
    FIELDS airlineid, name
    WHERE currencycode = 'EUR'
    INTO TABLE @DATA(carriers).

cl_demo_output=>display( carriers ).
```

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@EndUserText.label: 'Carrier Consumption View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@AccessControl.authorizationCheck: #CHECK
define view entity zmind2e_C_Carrier
  as select from ZMIND2E_I_Carrier
{
  key AirlineId,
      Name,
      CurrencyCode,
      /* Associations */
      _Currency
}
```

#### OpenSQL Select mit Parameter

```abap
SELECT * FROM cdsfrwk_open_so_items_by_taxr( pCuCo: 'EUR' ) INTO TABLE @DATA(result).
```

#### OpenSQL Select mit Assoziation

```abap
REPORT zmind2_select_association.

SELECT FROM zmind2e_c_carrier
    FIELDS
        airlineid,
        name,
        \_currency-currency
    INTO TABLE @DATA(carriers).

cl_demo_output=>display( carriers ).


SELECT FROM zmind2e_c_booking
    FIELDS
        bookingid,
        \_bookingsupplement[ (1) LEFT OUTER ]-supplementid,
        \_customer\_country\_text[ language = @sy-langu ]-countryname
    INTO TABLE @DATA(bookings).

cl_demo_output=>display( bookings ).
```

### ALV Integrated Data Access

```abap
REPORT zmind2_alv_ida.

DATA: carrier TYPE zmind2_carrier_id.
SELECT-OPTIONS so_carr FOR carrier.

END-OF-SELECTION.

  DATA(alv) = cl_salv_gui_table_ida=>create_for_cds_view(
      iv_cds_view_name = 'zmind2e_c_carrier' ).

  DATA(colletor) = NEW cl_salv_range_tab_collector( ).
  colletor->add_ranges_for_name(
      iv_name = 'AIRLINEID'
      it_ranges = so_carr[] ).
  colletor->get_collected_ranges(
      IMPORTING et_named_ranges = DATA(named_ranges) ).
  alv->set_select_options(
      it_ranges = named_ranges ).

  alv->fullscreen( )->display( ).
```

#### ALV IDA mit Parameter

```cds
REPORT zmind2_alv_ida_parameter.

DATA(alv) = cl_salv_gui_table_ida=>create_for_cds_view(
    iv_cds_view_name = 'zmind2e_c_parameter' ).

alv->set_view_parameters(
  it_parameters = VALUE #(
      ( name = 'P_Parameter' value = 'X' )
  ) ).

alv->fullscreen( )->display( ).
```

## Annotationen

### Annotationen Syntax

DDIC-based CDS View:

```cds
@AbapCatalog.sqlViewName: 'ZMS2IBOOKSUPPL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'Booking Supplement View - CDS data model'

@Search.searchable: true
@VDM.viewType: #BASIC

@ObjectModel.representativeKey: 'BookingSupplementID'
@Analytics.dataCategory: #DIMENSION
```

CDS View Entity:

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Booking Supplement View Entity'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #A,
    sizeCategory: #XXL,
    dataClass: #TRANSACTIONAL
}

@Search.searchable: true
@VDM.viewType: #BASIC

@ObjectModel.representativeKey: 'BookingSupplementID'
@Analytics.dataCategory: #DIMENSION

@AbapCatalog.entityBuffer.definitionAllowed: true
```

### Bezeichnertexte

```cds
...
  @EndUserText.label: 'Flugziel'
  @EndUserText.quickInfo: 'Zielland und -stadt des Fluges'
  concat_with_space(_DestinationAirport.CountryCode, _DestinationAirport.City, 1) as Destination,

  ...
```

### Einheitenreferenzen

```cds
...
  @Semantics.quantity.unitOfMeasure: 'DistanceUnit'
  cast(distance as abap.quan( 11, 0 )) as Distance,

  distance_unit                        as DistanceUnit,
...
```

### Währungsreferenzen

```cds
define view entity ZMIND2E_I_Flight
  as select from zmind2_flight
  association [0..1] to I_Currency as _Currency on $projection.CurrencyCode = _Currency.Currency
{
  key carrier_id            as AirlineID,
  key connection_id         as ConnectionID,
  key flight_date           as FlightDate,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      price                 as Price,

      @ObjectModel.foreignKey.association: '_Currency'
      currency_code         as CurrencyCode,

      // Weitere Felder

      // Assoziation für Fremdschlüsselbeziehung
      _Currency
}
```

### Kontrakt für Key-User-Felderweiterung 

```cds
@EndUserText.label: 'Demo for C0 released API'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true

@AbapCatalog.extensibility: {
  extensible: true,
  elementSuffix: 'EMO',
  allowNewDatasources: false,
  dataSources: ['Persistence'],
  quota: {
    maximumFields: 250,
    maximumBytes: 2500
  }
}
define view entity DEMO_CDS_PRODUCTTP_E
  as select from demo_product as Persistence
{
  key product_id
}
```

### Deprecation

```cds
...
  @VDM.lifecycle.status: #DEPRECATED
  @VDM.lifecycle.successor: 'Product'
  @API.element.releaseState: #DEPRECATED
  @API.element.successor: 'Product'
  Material,
...
```

### Access Control Annotation

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
```

### Fremdschlüsselbeziehung

CDS View mit Fremdschlüssel:

```cds
@ObjectModel.representativeKey: 'FlightDate'

define view entity ZMIND2E_I_Flight
  as select from zmind2_flight

  association [1]    to ZMIND2E_I_Carrier    as _Airline    on  $projection.AirlineID = _Airline.AirlineId
{
      @ObjectModel.foreignKey.association: '_Airline'
  key carrier_id            as AirlineID,
...
      _Airline,
...
}
```

CDS View mit repräsentativem Schlüssel:

```cds
@ObjectModel.representativeKey: 'AirlineId'

define view entity ZMIND2E_I_Carrier
  as select from zmind2_carrier
{
      @ObjectModel.text.element: ['Name']
  key carrier_id            as AirlineId,

      @Semantics.text: true
      name                  as Name,
...
}
```

### Textbeziehungen

#### Textbeziehung innerhalb einer View

```cds
define view entity ZMIND2E_I_Carrier
  as select from zmind2_carrier
{
      @ObjectModel.text.element: ['Name']
  key carrier_id            as AirlineId,

      @Semantics.text: true
      name                  as Name,
...
}
```

#### Textbeziehung mit Text View

```cds
define view entity ZMIND2E_I_TravelStatus
  as select from zmind2_trvl_stat
  association [0..*] to ZMIND2E_I_TravelStatusText as _Text on $projection.TravelStatus = _Text.TravelStatus
{
      @ObjectModel.text.association: '_Text'
  key travel_status as TravelStatus,

      _Text
}
```

```cds
@ObjectModel.dataCategory: #TEXT
define view entity ZMIND2E_I_TravelStatusText
  as select from zmind2_trvl_stxt
{
      @ObjectModel.text.element: ['Text']
  key travel_status as TravelStatus,
      @Semantics.language: true
  key language      as Language,
      @Semantics.text: true
      text          as Text
}
```

### Metadatenerweiterungen

Erweiterte CDS View Entity:

```cds
@Metadata.allowExtensions: true
define view entity ZREX_C_Flight
  as projection on ZREX_I_Flight
{
  ...
```

Metadatenwerweiterung:

```cds
@Metadata.layer: #PARTNER
annotate entity ZREX_C_Flight with
{
  @UI.facet: [{
      type: #COLLECTION,
      id: 'DetailsCollection',
      purpose: #STANDARD,
      label: 'Details',
      position: 10
    },{
      type: #FIELDGROUP_REFERENCE,
      id: 'FlightFieldGroup',
      targetQualifier: 'Flight',
      parentId: 'DetailsCollection',
      label: 'Flight',
      position: 10
    },{
      type: #FIELDGROUP_REFERENCE,
      id: 'AirplaneFieldGroup',
      targetQualifier: 'Airplane',
      parentId: 'DetailsCollection',
      label: 'Airplane',
      position: 20
    }]

  @UI.fieldGroup: [{ qualifier: 'Flight', position: 10 }]
  CarrierId;

  @UI.fieldGroup: [{ qualifier: 'Flight', position: 20 }]
  ConnectionID;

  @UI.fieldGroup: [{ qualifier: 'Flight', position: 30 }]
  @UI.lineItem: [{ qualifier: 'Flight', position: 10 }]
  FlightDate;

  @UI.fieldGroup: [{ qualifier: 'Flight', position: 40 }]
  Price;

  @UI.fieldGroup: [{ qualifier: 'Airplane', position: 10 }]
  @UI.lineItem: [{ qualifier: 'Flight', position: 20 }]
  PlaneType;
  
  @UI.fieldGroup: [{ qualifier: 'Airplane', position: 20 }]
  @UI.lineItem: [{ qualifier: 'Flight', position: 30 }]
  MaximumSeats;
  
  @UI.fieldGroup: [{ qualifier: 'Airplane', position: 30 }]
  @UI.lineItem: [{ qualifier: 'Flight', position: 40 }]
  OccupiedSeats;
}
```

## CDS View Erweiterung

```cds
@AbapCatalog.extensibility: {
  extensible: true,
  elementSuffix: 'ZAG',
  allowNewDatasources: false,
  dataSources: ['Agency'],
  quota: {
    maximumFields: 500,
    maximumBytes: 5000
  }
}

define view entity /DMO/E_Agency
  as select from /dmo/agency as Agency
{
  key agency_id as AgencyId
}
```

```cds
extend view entity /DMO/E_Agency with
{
  Agency./dmo/zzsloganzag as /DMO/ZZSloganZAG
}
```
## Entity Buffer

### Entity Buffer Annotation

```cds
@AbapCatalog.entityBuffer.definitionAllowed: true
```

### Entity Buffer Syntax

```cds
define view entity buffer on ZI_Flight
  layer customer
  type full
```

## Komposition

### Join

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View entity_client handling'
define view entity DEMO_CDS_CLIENT_HANDLING
  as select from t000 as client_independent
      left outer join demo_sales_order as _LeftOuter
        on _LeftOuter.id = client_independent.cccategory
      inner join demo_sales_order as _Inner
        on _Inner.id = client_independent.cccategory
      right outer join demo_sales_order as _RightOuter
        on _RightOuter.id = client_independent.cccategory
    association [0..1] to demo_sales_order as _Assoc
      on _Assoc.id = client_independent.cccategory
    {
      key _Assoc.so_key,
          _Assoc.id,
          _Inner.id      as id_inner,
          _LeftOuter.id  as id_LeftOuter,
          _RightOuter.id as id_RightOuter,
          client_independent.cccategory
    }
    where client_independent.mtext = abap.char'SAP AG Konzern'

```

### Assoziationen

#### Syntax

```cds
define view entity ZMIND2E_I_Booking
  as select from zmind2_booking

  association [1..1] to ZMIND2E_I_Travel            as _Travel            on  $projection.TravelID = _Travel.TravelID
  association [0..*] to ZMIND2E_I_BookingSupplement as _BookingSupplement on  $projection.BookingID = _BookingSupplement.BookingID
                                                                          and $projection.TravelID  = _BookingSupplement.TravelID
  association [1..1] to ZMIND2E_I_Customer          as _Customer          on  $projection.CustomerID = _Customer.CustomerId
  association [1..1] to ZMIND2E_I_Carrier           as _Carrier           on  $projection.AirlineID = _Carrier.AirlineId
  association [1..1] to ZMIND2E_I_Connection        as _Connection        on  $projection.AirlineID    = _Connection.AirlineId
                                                                          and $projection.ConnectionID = _Connection.ConnectionId
  association [1..1] to I_Currency                  as _Currency          on  $projection.CurrencyCode = _Currency.Currency

{
      @ObjectModel.foreignKey.association: '_Travel'
      @Search.defaultSearchElement: true
  key travel_id     as TravelID,

      @Search.defaultSearchElement: true
  key booking_id    as BookingID,
      booking_date  as BookingDate,

      @Consumption.valueHelpDefinition: [{entity: {name: 'ZMIND2E_I_Customer', element: 'CustomerId' }}]
      @Search.defaultSearchElement: true
      @ObjectModel.foreignKey.association: '_Customer'
      customer_id   as CustomerID,

      @ObjectModel.foreignKey.association: '_Carrier'
      carrier_id    as AirlineID,

      @ObjectModel.foreignKey.association: '_Connection'
      connection_id as ConnectionID,

      flight_date   as FlightDate,

      @Semantics.amount.currencyCode: 'CurrencyCode'
      flight_price  as FlightPrice,

      @ObjectModel.foreignKey.association: '_Currency'
      currency_code as CurrencyCode,

      /* Associations */
      _Travel,
      _BookingSupplement,
      _Customer,
      _Carrier,
      _Connection,
      _Currency
}
```

#### Komplexe Komposition

```cds
...
_Currency._Text[ inner where Language = 'D' ].CurrencyName      as CurrencyNameGerman,
_Currency._Text[ left outer where Language = 'I' ].CurrencyName as CurrencyNameItalian,
...
```

### Union

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.ignorePropagatedAnnotations: true
@AbapCatalog.viewEnhancementCategory: [#PROJECTION_LIST,#UNION]
define view entity DEMO_CDS_UNION_VE
  as select from
    demo_join1
    {
      a as c1,
      b as c2
    }
union select from
  demo_join2
    {
      d as c1,
      e as c2
    }
union all select from
  demo_join3
    {
      i as c1,
      j as c2
    }
```

## Access Control

### Access Control Syntax

```cds
@EndUserText.label: 'Carrier'
@MappingRole: true
define role ZMIND2E_C_CARRIER {
  grant select on ZMIND2E_C_CARRIER
    where (AirlineId) = aspect pfcg_auth(S_CARRID, CARRID, ACTVT = '03' );
}
```

### Access Control Vererbung

```cds
@MappingRole: true
define role ZC_SalesOrder {
  grant select on ZC_SalesOrder
    where inheriting conditions from entity ZI_SalesOrder;
}
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
@EndUserText.label: 'Service definition for FuncLoc Odata'
define service API_FUNCTIONALLOCATION {
  expose A_FunctionalLocation as FunctionalLocation;
  expose A_FunctionalLocationLongText as FunctionalLocationLongText;
  expose A_FunctionalLocationPartner as FunctionalLocationPartner;
  expose A_FuncnlLocClfnClass as FunctionalLocationClass;
  expose A_FuncnlLocClfnClassChar as Characteristic;
  expose A_FuncnlLocClfnCharValue as Value;
  expose A_FuncnlLocWarrantyAssgmt as FunctionalLocationWarranty;
}
```

## Fiori Elements

### Line Item

```cds
@AbapCatalog.sqlViewName: 'ZMS2EXCBOOKSUPPL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Booking Supplement Consumption View'

define view ZMIND2EX_C_BookingSupplement
  as select from ZMind2Ex_I_BookingSupplement
{
  key TravelID,
  key BookingID,
  key BookingSupplementID,
      @UI.lineItem: [{ position: 20 }]
      SupplementID,
      @UI.lineItem: [{ position: 10 }]
      SupplementType,
      @UI.lineItem: [{ position: 30 }]
      Price,
      CurrencyCode,
      /* Associations */
      _Booking,
      _SupplementText
}
```

### Freitextsuche

```cds
@Search.searchable: true

define view entity ZMIND2E_I_Flight
  as select from zmind2_flight
{
      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @ObjectModel.foreignKey.association: '_Airline'
  key carrier_id            as AirlineID,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      @ObjectModel.foreignKey.association: '_Connection'
  key connection_id         as ConnectionID,

  key flight_date           as FlightDate,
...
}
```

### Filter

```cds
...
  @UI.selectionField: [{ position: 10 }]
  @Consumption.filter: {
    selectionType: #INTERVAL,
    mandatory: true
  }
  EndDate,
...
```

### Suchhilfen

Einfache Suchhilfe:

```cds
@Consumption.valueHelpDefinition: [{ entity.name: 'I_CurrencyStdVH', entity.element: 'Currency' }]
CurrencyCode
```

Suchhilfe mit erweiterten Abhängigkeiten:

```cds
...
@Consumption.valueHelpDefinition: [{
  entity: { name: 'ZMIND2E_I_FlightVH', element: 'AirlineID' },
  additionalBinding: [
    { element: 'ConnectionID', localElement: 'ConnectionId', usage: #RESULT },
    { element: 'FlightDate', localElement: 'FlightDate', usage: #RESULT }
  ]
}]
CarrierId,

@Consumption.valueHelpDefinition: [{
  entity: { name: 'ZMIND2E_I_FlightVH', element: 'ConnectionID' },
  additionalBinding: [
    { element: 'AirlineID', localElement: 'CarrierId', usage: #FILTER_AND_RESULT },
    { element: 'FlightDate', localElement: 'FlightDate', usage: #RESULT }
  ]
}]
ConnectionId,

@Consumption.valueHelpDefinition: [{
  entity: { name: 'ZMIND2E_I_FlightVH', element: 'FlightDate' },
  additionalBinding: [
    { element: 'AirlineID', localElement: 'CarrierId', usage: #FILTER_AND_RESULT },
    { element: 'ConnectionID', localElement: 'ConnectionId', usage: #FILTER_AND_RESULT }
  ]
}]
FlightDate,
...
```

### Tabellenbezeichnung

```cds
@UI.headerInfo: {
  typeName: 'Booking',
  typeNamePlural: 'Bookings'
}
```

### Semantische Hervorhebung

```cds
@Metadata.layer: #CUSTOMER
@UI.headerInfo.title.criticality: 'StatusCriticality'
annotate view ZMIND2RAP_C_Travel with
{
  @UI.lineItem: [
    { exclude: true },
    { position: 50, criticality: 'StatusCriticality' }
  ]
  @UI.fieldGroup: [{ qualifier: 'Details', position: 40, criticality: 'StatusCriticality' }]
  @UI.textArrangement: #TEXT_ONLY
  Status;

  @UI.hidden: true
  StatusCriticality;
}
```

### Gruppieren & Sortieren

```cds
@UI.presentationVariant: [{ 
    sortOrder: [{ by: 'FlightDate', direction: #ASC }],
    groupBy: ['CarrierId']
}]

annotate view ZMIND2RAP_C_Booking with
{
...
```

### Seitentitel

```cds
@UI.headerInfo: {
    typeName: 'Travel',
    typeNamePlural: 'Travels',
    title.value: 'Description',
    description.value: 'CustomerId'
}
```

### Facetten

```cds
 @UI.facet: [{
    type: #COLLECTION,
    id: 'TravelCollection',
    label: 'Travel',
    purpose: #STANDARD,
    position: 10
  },{
    type: #FIELDGROUP_REFERENCE,
    id: 'DetailsFieldGroup',
    targetQualifier: 'Details',
    parentId: 'TravelCollection',
    label: 'Details',
    position: 10
  },{
    type: #COLLECTION,
    id: 'CustomerCollection',
    label: 'Customer',
    purpose: #STANDARD,
    position: 20
  },{
    type: #FIELDGROUP_REFERENCE,
    id: 'CustomerFieldGroup',
    parentId: 'CustomerCollection',
    targetQualifier: 'Customer',
    label: 'Customer',
    position: 10
  },{
    type: #LINEITEM_REFERENCE,
    id: 'BookingLineItem',
    purpose: #STANDARD,
    targetElement: '_Booking',
    targetQualifier: 'Booking',
    position: 30,
    label: 'Bookings'
  },{
    type: #FIELDGROUP_REFERENCE,
    id: 'TraceFieldGroup',
    targetQualifier: 'Trace',
    purpose: #STANDARD,
    label: 'Trace',
    position: 40
  },{
    type: #FIELDGROUP_REFERENCE,
    id: 'DatesFieldGroup',
    targetQualifier: 'Dates',
    parentId: 'TravelCollection',
    label: 'Date',
    position: 20
  },{
    type: #FIELDGROUP_REFERENCE,
    id: 'PricesFieldGroup',
    targetQualifier: 'Prices',
    parentId: 'TravelCollection',
    label: 'Price',
    position: 40
  }]
```

## Virtuelle Elemente

### Annotationen für virtuelle Elemente

```cds
define view entity /DMO/C_Booking_VE
  as projection on /DMO/I_Booking_U
{ ...
          @ObjectModel.virtualElementCalculatedBy: 'ABAP:/DMO/CL_DAYS_TO_FLIGHT'
          @EndUserText.label: 'Days to Flight'
  virtual DaysToFlight : abap.int2,
  …}
```

### Virtuelle Elemente: Datenberechnung

```abap
CLASS /dmo/cl_days_to_flight DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_sadl_exit_calc_element_read.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS /dmo/cl_days_to_flight IMPLEMENTATION.
  METHOD if_sadl_exit_calc_element_read~get_calculation_info.
    IF iv_entity <> '/DMO/C_BOOKING_VE'.
      RAISE EXCEPTION TYPE /dmo/cx_virtual_elements
        EXPORTING
          textid = /dmo/cx_virtual_elements=>entity_not_known
          entity = iv_entity.
    ENDIF.

    LOOP AT it_requested_calc_elements ASSIGNING FIELD-SYMBOL(<fs_calc_element>).
      CASE <fs_calc_element>.
        WHEN 'DAYSTOFLIGHT'.
          APPEND 'FLIGHTDATE' TO et_requested_orig_elements.

*        WHEN 'ANOTHERELEMENT'.
*          APPEND '' ...

        WHEN OTHERS.
          RAISE EXCEPTION TYPE /dmo/cx_virtual_elements
            EXPORTING
              textid  = /dmo/cx_virtual_elements=>virtual_element_not_known
              element = <fs_calc_element>
              entity  = iv_entity.
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_sadl_exit_calc_element_read~calculate.
    DATA(lv_today) = cl_abap_context_info=>get_system_date( ).

    DATA lt_original_data TYPE STANDARD TABLE OF /dmo/c_booking_proc_ve WITH DEFAULT KEY.
    lt_original_data = CORRESPONDING #( it_original_data ).

    LOOP AT lt_original_data ASSIGNING FIELD-SYMBOL(<fs_original_data>).
      <fs_original_data>-DaysToFlight =  <fs_original_data>-FlightDate - lv_today.
    ENDLOOP.

    ct_calculated_data = CORRESPONDING #(  lt_original_data ).
  ENDMETHOD.
ENDCLASS.
```

### Virtuelle Elemente: Filterung

```cds
define view <CdsConsumptionView> 
		as select from <data_source>
{
		...    
		@ObjectModel.filter.transformedBy: 'ABAP:<code_exit_class>'
		cast( '' as <dtype> preserving type) as <view.element>
		...
} 
```

```abap
CLASS zcl_filter_discount DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES:
      if_sadl_exit_filter_transform.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_filter_discount IMPLEMENTATION.

  METHOD if_sadl_exit_filter_transform~map_atom.
    IF iv_element <> 'GROSSAMOUNTWITHDISCOUNT'.
      RAISE EXCEPTION TYPE zcx_filter_exit EXPORTING textid = zcx_filter_exit=>element_not_expected.
    ENDIF.

    DATA(lo_cfac) = cl_sadl_cond_prov_factory_pub=>create_simple_cond_factory( ).
    DATA(amount) = lo_cfac->element( 'CONVERTEDGROSSAMOUNT' ).
    DATA(lv_originalvalue) = 1000 + ( iv_value - 1000 ) / '0.9'.

    CASE iv_operator.

      WHEN if_sadl_exit_filter_transform~co_operator-equals.

        ro_condition = amount->less_than(    1000 )->and( amount->equals( iv_value )
                )->or( amount->greater_than( 1000 )->and( amount->equals( lv_originalvalue ) ) ).

      WHEN if_sadl_exit_filter_transform~co_operator-less_than.

        ro_condition = amount->less_than(    1000 )->and( amount->less_than( iv_value ) 
                )->or( amount->greater_than( 1000 )->and( amount->less_than( lv_originalvalue ) ) ).

      WHEN if_sadl_exit_filter_transform~co_operator-greater_than.
        ro_condition = amount->less_than(    1000 )->and( amount->greater_than( iv_value ) 
                )->or( amount->greater_than( 1000 )->and( amount->greater_than( lv_originalvalue ) ) ).

      WHEN if_sadl_exit_filter_transform~co_operator-is_null.
        ro_condition = amount->is_null( ).

      WHEN if_sadl_exit_filter_transform~co_operator-covers_pattern.
        RAISE EXCEPTION TYPE zcx_filter_exit.

    ENDCASE.
  ENDMETHOD.
```

### Virtuelle Elemente: Sortierung

```cds
define view <CdsConsumptionView> 
		as select from <data_source>
{
		...    
		@ObjectModel.sort.transformedBy: 'ABAP:<code_exit_class>'
		cast( '' as <dtype> preserving type) as <view.element>
		...
}
```

```abap
CLASS zcl_sort_discount DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
      INTERFACES:
      if_sadl_exit_sort_transform.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sort_discount IMPLEMENTATION.


  METHOD if_sadl_exit_sort_transform~map_element.
    IF iv_element <> 'GROSSAMOUNTWITHDISCOUNT'.
      RAISE EXCEPTION TYPE zcx_sorting_exit EXPORTING textid = zcx_sorting_exit=>element_not_expected.
    ENDIF.
      APPEND value #( name = `CONVERTEDGROSSAMOUNT` ) TO et_sort_elements.
  ENDMETHOD.

ENDCLASS.
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
define view entity ZMIND2_I_FuncLocRel
  as select from I_FunctionalLocation
  association [0..1] to ZMIND2_I_FuncLocRel as _Parent on $projection.SuperiorFunctionalLocation = _Parent.FunctionalLocation
{
  key FunctionalLocation,
      SuperiorFunctionalLocation,
      _FunctionalLocationText[1:Language=$session.system_language].FunctionalLocationName,

      _Parent
}
```

```cds
define hierarchy ZMIND2_I_FuncLocationHierarchy
  as parent child hierarchy(
    source ZMIND2_I_FuncLocRel
    child to parent association _Parent
    siblings order by
      FunctionalLocation ascending
    orphans root
  )
{
  key FunctionalLocation,
      FunctionalLocationName,
      SuperiorFunctionalLocation,
      $node.parent_id             as ParentNode,
      $node.node_id               as NodeId,
      $node.hierarchy_is_cycle    as IsCycle,
      $node.hierarchy_is_orphan   as IsOrphan,
      $node.hierarchy_level       as HierarchyLevel,
      $node.hierarchy_rank        as HierarchyRank,
      $node.hierarchy_parent_rank as HierarchyParentRank,
      $node.hierarchy_tree_size   as HierarchyTreeSize
}
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
environment->get_access_control_double( )->disable_access_control( ).
```

#### Test unberechtigter Nutzer

```abap
data(acm_data_no_authorization) = cl_cds_test_data=>create_access_control_data( i_role_authorizations = VALUE #( ) ).
environment->get_access_control_double( )->enable_access_control( i_access_control_data = acm_data_no_authorization ).
```

#### Test: Berechtigungen mocken

```abap
FINAL(acm_data_no_authorization) = cl_cds_test_data=>create_access_control_data(
                                        i_role_authorizations = VALUE #(
                                            ( object         = 'ZX_CARRIER'
                                              authorizations = VALUE #( ( VALUE #( ( fieldname   = 'ZX_CARRIER'
                                                                                    fieldvalues = VALUE #(
                                                                                        ( lower_value = 'LH' ) ) )
                                                                                  ( fieldname   = 'ACTVT'
                                                                                    fieldvalues = VALUE #(
                                                                                        ( lower_value = '03' ) ) ) ) ) ) ) ) ).
environment->get_access_control_double( )->enable_access_control(  i_access_control_data = acm_data_no_authorization ).
```
