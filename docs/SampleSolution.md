<img src="https://mindsquare.de/files/logo-mindsquare-176x781.png" alt="mindsquare Logo" title="mindsquare AG" align="right">

# Sample Solutions of the mindsquare CDS Training Unit

Sample solutions of the [mindsquare CDS View Training Unit](https://mindsquare.de/schulungen/)

**!!! IMPORTANT !!!**

There are preconditions listed for every step before you are able to execute the code in case you want to test it for yourself.
The preconditions don't list everything absolutely necessary for the code to be functional, mostly just the last step (else the preconditions would grow to unreadable levels with every step). Example:

To define ZC_CustomerName (the Consumption View), the precondition lists ZI_CustomerName (Basic Interface View) for the code to work. But for ZI_CustomerName to work, you need to define ZI_Customer (Basic Interface View) first (and everything else listed for every step).

So, if necessary, follow the chain to the beginning and start copying from there if you haven't done so yet. Use the search function (CTRL + F) for easier navigation.

## Defining data models

### Basic Interface View

Preconditions for this step:
- None
- (Have the necessary data tables defined)

#### Defining ZI_Customer

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic Interface View for Customer'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MS_Customer
  as select from zmind2_customer
{
  key customer_id   as CustomerId,
      first_name    as FirstName,
      last_name     as LastName,
      title         as Title,
      street        as Street,
      postal_code   as PostalCode,
      city          as City,
      country_code  as CountryCode,
      phone_number  as PhoneNumber,
      email_address as EmailAddress,
      createdby     as CreatedBy,
      createdat     as CreatedAt,
      lastchangedby as LastChangedBy,
      lastchangedat as LastChangedAt
}
```

### Composite Interface View

Preconditions for this step:
- Have ZI_Customer defined (Basic Interface View)

#### Defining ZI_CustomerName 

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composite Interface View for Customer'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MS_CustomerName
  as select from ZI_MS_Customer
{
  key CustomerId,
      FirstName,
      LastName,
      concat_with_space( FirstName, LastName, 1 ) as FullName,
      case Title
          when 'Mr.'  then 'M'
          when 'Mrs.' then 'F'
          else ''
      end                                         as Gender,                                       as Gender,
      Title,
      Street,
      PostalCode,
      City,
      CountryCode,
      PhoneNumber,
      EmailAddress,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt
}
```

### Consumption View

Preconditions for this step:
- Have ZI_CustomerName defined (Composite Interface View)

#### Defining ZC_CustomerName

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for CustomerName'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZC_MS_CustomerName
  as select from ZI_MS_CustomerName
{
  key CustomerId,
      FirstName,
      LastName,
      FullName,
      Gender,
      Title,
      Street,
      PostalCode,
      City,
      CountryCode,
      PhoneNumber,
      EmailAddress,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt
}
```

### Parameter - Composite Interface View

Preconditions for this step:
- Have ZI_CustomerName defined (Composite Interface View)

#### Defining ZI_BookingCurrency
```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composite Interface View for Booking'
define view entity ZI_MS_BookingCurrency
  with parameters
    pDesiredCurrency : abap.cuky(5)
  as select from ZMIND2E_I_Booking
{
  key TravelID,
  key BookingID,
      BookingDate,
      CustomerID,
      CarrierId,
      BookingStatus,
      ConnectionID,
      FlightDate,
      @EndUserText.label: 'Old Flight Price'
      cast(FlightPrice as abap.dec(17,2)) as FlightPrice,
      @EndUserText.label: 'Old Currency Code'
      CurrencyCode,

      @EndUserText.label: 'Flight Price in Desired Currency'
      cast(currency_conversion(
          amount => FlightPrice,
          source_currency => CurrencyCode,
          target_currency => $parameters.pDesiredCurrency,
          exchange_rate_date => FlightDate
      ) as abap.dec(17,2))                as ConvertedFlightPrice,

      $parameters.pDesiredCurrency        as NewCurrencyCode,

      LocalLastChangedAt,
      /* Associations */
      _BookingSupplement,
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _Status,
      _Travel
}
```

### Supplements

#### Defining ZIX_BookingSupplement

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic Interface View Supplements'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZIX_MS_BookingSupplement
  as select from zmind2_book_supp
{
  key travel_id             as TravelId,
  key booking_id            as BookingId,
  key booking_supplement_id as BookingSupplementId,
      supplement_id         as SupplementId,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      price                 as Price,
      currency_code         as CurrencyCode,
      last_changed_at       as LastChangedAt
}
```

#### Defining ZI_BookingSupplementName

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composite Interface View Supplement'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@Metadata.allowExtensions: true
@VDM.viewType: #COMPOSITE
define view entity ZI_MS_BookingSupplementName
  as select from ZIX_MS_BookingSupplement

{
  key TravelId,
  key BookingId,
  key BookingSupplementId,
      SupplementId,
      case
        when SupplementId like 'BV%' then 'Beverage'
        when SupplementId like 'ML%' then 'Meal'
        when SupplementId like 'LU%' then 'Luggage'
        when SupplementId like 'EX%' then 'Extras'
        else 'Unknown'
      end as SupplementDesc,
      @Semantics.amount.currencyCode: 'CurrencyCode'
      Price,
      CurrencyCode,
      LastChangedAt
}
```

## Associations

### Extending existing Views with Associations

Changes:
- ZI_Customer
  - Added Association to I_Country
  - Exposed I_Country to the other Views
- ZI_CustomerName
  - Exposed I_Country
- ZC_CustomerName
  - Exposed I_Country
  - Added the field name of country in german language
- ZI_BookingCurrency
  - Added Association to ZI_CustomerName (Composite View)

#### Adjusting ZI_Customer

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic Interface View for Customer'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MS_Customer
  as select from zmind2_customer
  association [0..1] to I_Country as _Country on $projection.CountryCode = _Country.Country
{
  key customer_id   as CustomerId,
      first_name    as FirstName,
      last_name     as LastName,
      title         as Title,
      street        as Street,
      postal_code   as PostalCode,
      city          as City,
      country_code  as CountryCode,
      phone_number  as PhoneNumber,
      email_address as EmailAddress,
      createdby     as CreatedBy,
      createdat     as CreatedAt,
      lastchangedby as LastChangedBy,
      lastchangedat as LastChangedAt,

      _Country
}
```

#### Adjusting ZI_CustomerName

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composite Interface View for Customer'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MS_CustomerName
  as select from ZI_MS_Customer
{
  key CustomerId,
      FirstName,
      LastName,
      concat_with_space( FirstName, LastName, 1 ) as FullName,
      cast(
        case Title
            when 'Mr.'  then 'M'
            when 'Mrs.' then 'F'
            else ''
        end as zmind2_gender
      )                                           as Gender,
      Title,
      Street,
      PostalCode,
      City,
      CountryCode,
      PhoneNumber,
      EmailAddress,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,

      _Country
}
```

#### Adjusting ZC_CustomerName

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for CustomerName'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZC_MS_CustomerName
  as select from ZI_MS_CustomerName
{
  key CustomerId,
      FirstName,
      LastName,
      FullName,
      Gender,
      Title,
      Street,
      PostalCode,
      City,
      CountryCode,
      _Country._Text[inner where Language = 'D'].CountryName as CountryNameGerman,
      PhoneNumber,
      EmailAddress,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,

      _Country
}

```

#### Adjusting ZI_BookingCurrency

```cds
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composite Interface View for Booking'
define view entity ZI_MS_BookingCurrency
  with parameters
    P_DesiredCurrency : abap.cuky(5)
  as select from ZMIND2E_I_Booking
  association [0..1] to ZI_MS_CustomerName as _CustomerName on $projection.CustomerID = _CustomerName.CustomerId
{
  key TravelID,
  key BookingID,
      BookingDate,
      CustomerID,
      CarrierId,
      BookingStatus,
      ConnectionID,
      FlightDate,
      @EndUserText.label: 'Old Flight Price'
      cast(FlightPrice as abap.dec(17,2)) as FlightPrice,
      @EndUserText.label: 'Old Currency Code'
      CurrencyCode,

      @EndUserText.label: 'Flight Price in Desired Currency'
      cast(currency_conversion(
          amount => FlightPrice,
          source_currency => CurrencyCode,
          target_currency => $parameters.P_DesiredCurrency,
          exchange_rate_date => FlightDate
      ) as abap.dec(17,2))                as ConvertedFlightPrice,

      $parameters.P_DesiredCurrency        as NewCurrencyCode,

      LocalLastChangedAt,
      /* Associations */
      _BookingSupplement,
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _Status,
      _Travel,
      _CustomerName
}
```

## View Extensions

### Direct Extensions

#### Defining ZXI_Agency

Preconditions for this step:
- None

```cds
extend view entity ZMIND2E_I_Agency with
{
  zmind2_agency.zz_ext_1 as MyExtensionField
}
```

#### Defining ZXI_Customer

Preconditions for this step:
- Have ZI_Customer defined
- Annotation "@Metadata.allowExtensions: true" has been added to ZI_Customer
- Annotation value of "@AbapCatalog.viewEnhancementCategory" in ZI_Customer has been set to "[#PROJECTION_LIST]"

```cds
extend view entity ZI_MS_Customer with
{
  'Extension Value 1' as Extension1,
  'X'                 as Extension2
}
```

## oData Services

Preconditions for this step:
- Have ZI_CustomerName defined
- Have ZI_BookingCurrency defined

### Define Consumption Views

#### Defining ZC_Booking (and associating ZC_Customer)

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for BookingCurrency'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZC_MS_Booking
  as select from ZI_MS_BookingCurrency(P_DesiredCurrency: 'EUR')
  association [0..1] to ZC_MS_Customer as _Customer on $projection.CustomerID = _Customer.CustomerId
{
  key TravelID,
  key BookingID,
      BookingDate,
      CustomerID,
      CarrierId,
      BookingStatus,
      ConnectionID,
      FlightDate,
      FlightPrice,
      CurrencyCode,
      ConvertedFlightPrice,
      NewCurrencyCode,
      LocalLastChangedAt,

      /* Associations */
      _BookingSupplement,
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _CustomerName,
      _Status,
      _Travel
}
```

#### Defining ZC_Customer

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for CustomerName'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZC_MS_Customer
  as select from ZI_MS_CustomerName
{
  key CustomerId,
      FirstName,
      LastName,
      FullName,
      Gender,
      Title,
      Street,
      PostalCode,
      City,
      CountryCode,
      PhoneNumber,
      EmailAddress,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,

      /* Associations */
      _Country
}
```

#### Defining Z_UI_Booking

```cds
@EndUserText.label: 'Service Definition for Booking'
define service ZUI_MS_Booking {
  expose ZC_MS_Booking;
}
```

### Service Definition
@EndUserText.label: 'Service Definition for Booking'
define service ZUI_MS_Booking {
  expose ZC_MS_Booking  as Booking;
  expose ZC_MS_Customer as Customer;
}

### Service Binding

Note:
- For Service Bindings, please look at the slides of the training unit

## Annotations

Changes:
- Added a header
- Added UI.lineItem to several fields
- Added UI.selectionField to several fields
- Added search help to several fields
- Added case, when status in travelassociation = 'B', color the field green, otherwise nothing

### Adjusting ZC_Booking

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@UI.headerInfo:{
typeName: 'Buchung',
typeNamePlural: 'Buchungen',
title.value: '_Customer.FullName',
description.value: '_Connection._Airline.Name'

}
define view entity ZC_MS_Booking
  as select from ZI_MS_BookingCurrency(pDesiredCurrency : 'EUR')
  association [0..1] to ZC_MS_Customer as _Customer on $projection.CustomerID = _Customer.CustomerId
{
      @UI.lineItem: [{
      position: 10,
      criticality: 'StatusCriticality'
      }]
  key TravelID,
  key BookingID,
      BookingDate,
      @UI.lineItem: [{
      position: 30
      }]
      @Consumption.valueHelpDefinition: [{
            entity: { name: 'ZC_MS_Customer', element: 'CustomerId'}
      }]
      @UI.selectionField: [{
        position: 20
       }]
      CustomerID,
      @UI.lineItem: [{
      position: 40
      }]
      CarrierId,
      @UI.lineItem: [{
      position: 50
      }]
      _Connection.DepartureAirport   as DepartureAirport,
      @UI.lineItem: [{
      position: 60
      }]
      @Consumption.valueHelpDefinition: [{
        entity: { name: 'ZMIND2E_I_Connection', element: 'DestinationAirport' }
      }]
      @UI.selectionField: [{
        position: 30
       }]
      _Connection.DestinationAirport as DestinationAirport,
      BookingStatus,
      ConnectionID,
      @UI.lineItem: [{
      position: 20
      }]
      @UI.selectionField: [{
        position: 10
       }]
      @Consumption.filter: {
        selectionType: #INTERVAL,
        mandatory: true
      }
      FlightDate,
      @UI.lineItem: [{
      position: 70
      }]
      FlightPrice,
      OldCurrencyCode,
      ConvertedFlightPrice,
      NewCurrencyCode,
      LocalLastChangedAt,
      /* Associations */
      _BookingSupplement,
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _CustomerName,
      _Status,
      _Travel,


      // Criticality Column
      case
          when _Travel.Status = 'B' then 3
          else 0
      end                            as StatusCriticality
}

```

#### ZI_Adding BookingSupplement

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Basic View for BookingSupplement'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MS_BookingSupplement
  as select from zmind2_book_supp
{
  key travel_id             as TravelId,
  key booking_id            as BookingId,
  key booking_supplement_id as BookingSupplementId,
      supplement_id         as SupplementId,
      price                 as Price,
      currency_code         as CurrencyCode,
      last_changed_at       as LastChangedAt
}
```

#### Adding ZI_BookingSupplementName

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composite View for BookingSupplement'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MS_BookingSupplementName
  as select from ZI_MS_BookingSupplement
{
  key TravelId,
  key BookingId,
  key BookingSupplementId,
      SupplementId,
      case
        when SupplementId like 'BV%' then 'Beverage'
        when SupplementId like 'ML%' then 'Meal'
        when SupplementId like 'LU%' then 'Luggage'
        when SupplementId like 'EX%' then 'Extras'
        else 'Unknown'
      end as SupplementDesc,
      Price,
      CurrencyCode,
      LastChangedAt
}
```

Changes:
- Added association to ZI_BookingSupplementName

### Adjusting ZC_Booking

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for BookingCurrency'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@UI.headerInfo:{
typeName: 'Booking',
typeNamePlural: 'Bookings',
title.value: '_Customer.FullName',
description.value: '_Connection._Airline.Name'

}

define view entity ZC_MS_Booking
  as select from ZI_MS_BookingCurrency(pDesiredCurrency: 'EUR')
  association [0..1] to ZC_MS_Customer              as _Customer on  $projection.CustomerID = _Customer.CustomerId
  association [0..*] to ZI_MS_BookingSupplementName as _BookSupp on  $projection.TravelID  = _BookSupp.TravelId
                                                                 and $projection.BookingID = _BookSupp.BookingId
{

      @UI.lineItem: [{
      position: 10,
      criticality: 'StatusCriticality'
      }]
  key TravelID,
  key BookingID,
      BookingDate,

      @UI.lineItem: [{
      position: 30
      }]
      @Consumption.valueHelpDefinition: [{
            entity: { name: 'ZC_MS_Customer', element: 'CustomerId'}
      }]
      @UI.selectionField: [{
        position: 20
       }]
      CustomerID,

      @UI.lineItem: [{
      position: 40
      }]
      CarrierId,

      @UI.lineItem: [{
      position: 50
      }]
      _Connection.DepartureAirport                as DepartureAirport,

      @UI.lineItem: [{
      position: 60
      }]
      _Connection.DestinationAirport              as DestinationAirport,

      @UI.selectionField: [{
        position: 30
      }]
      @Consumption.valueHelpDefinition: [{
            entity: { name: 'ZMIND2E_I_Connection', element: 'Country'}
      }]
      _Connection._DestinationAirport.CountryCode as DestinationCountry,
      BookingStatus,
      ConnectionID,

      @UI.lineItem: [{
      position: 20
      }]
      @UI.selectionField: [{
        position: 10
       }]
      @Consumption.filter: {
        selectionType: #INTERVAL,
        mandatory: true
      }
      FlightDate,

      @UI.lineItem: [{
      position: 70
      }]
      FlightPrice,

      CurrencyCode,
      ConvertedFlightPrice,
      NewCurrencyCode,
      LocalLastChangedAt,

      /* Associations */
      _BookingSupplement,
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _CustomerName,
      _Status,
      _Travel,
      _BookSupp,

      // Criticality Column
      case
          when _Travel.Status = 'B' then 3
          else 0
      end                                         as StatusCriticality
}
```

### Adding further annotations

Changes:
- ZC_Booking
  - Adjusted the header info
  - Added a collection
  - Added 3 fieldgroups
  - Added fields to the fieldgroups
- ZI_BookingSupplementName
  - Added annotations

#### Adjusting ZC_Booking

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for BookingCurrency'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@UI.headerInfo: {
typeName: 'Booking',
    typeNamePlural: 'Bookings',
    title: {
        value: '_Customer.FullName',
        type: #STANDARD
    },
    description: {
        value: 'FlightDate',
        type: #STANDARD
    }

}

define view entity ZC_MS_Booking
  as select from ZI_MS_BookingCurrency(pDesiredCurrency: 'EUR')
  association [0..1] to ZC_MS_Customer              as _Customer on  $projection.CustomerID = _Customer.CustomerId
  association [0..*] to ZI_MS_BookingSupplementName as _BookSupp on  $projection.TravelID  = _BookSupp.TravelId
                                                                 and $projection.BookingID = _BookSupp.BookingId
{

      @UI.facet: [{
      type: #COLLECTION,
      id: 'CustomerCollection',
      label: 'Customer Information',
      purpose: #STANDARD,
      position: 10
      },{
      type: #FIELDGROUP_REFERENCE,
      id: 'GeneralCustomerData',
      targetQualifier: 'GeneralCustomerData',
      parentId: 'CustomerCollection',
      label: 'General Customer Data',
      position: 10
      },{
      type: #FIELDGROUP_REFERENCE,
      id: 'CustomerAddress',
      targetQualifier: 'CustomerAddress',
      parentId: 'CustomerCollection',
      label: 'Adresse',
      position: 20
      },{
      type: #LINEITEM_REFERENCE,
      id: 'AdditionalBookings',
      targetQualifier: 'AdditionalBookings',
      purpose: #STANDARD,
      label: 'Additional Bookings',
      targetElement: '_BookSupp',
      position: 30
      }]

      @UI.lineItem: [{
      position: 10,
      criticality: 'StatusCriticality'
      }]
  key TravelID,
  key BookingID,
      BookingDate,

      @UI.lineItem: [{
      position: 30
      }]
      @Consumption.valueHelpDefinition: [{
            entity: { name: 'ZC_MS_Customer', element: 'CustomerId'}
      }]
      @UI.selectionField: [{
        position: 20
       }]
      @UI.fieldGroup: [{ qualifier: 'GeneralCustomerData', position: 10 }]
      CustomerID,

      @UI.fieldGroup: [{ qualifier: 'GeneralCustomerData', position: 20 }]
      _Customer.FullName,

      @UI.lineItem: [{
      position: 40
      }]
      CarrierId,

      @UI.lineItem: [{
      position: 50
      }]
      _Connection.DepartureAirport                as DepartureAirport,

      @UI.lineItem: [{
      position: 60
      }]
      _Connection.DestinationAirport              as DestinationAirport,

      @UI.selectionField: [{
        position: 30
      }]
      @Consumption.valueHelpDefinition: [{
            entity: { name: 'ZMIND2E_I_Connection', element: 'Country'}
      }]
      _Connection._DestinationAirport.CountryCode as DestinationCountry,
      BookingStatus,
      ConnectionID,

      @UI.lineItem: [{
      position: 20
      }]
      @UI.selectionField: [{
        position: 10
       }]
      @Consumption.filter: {
        selectionType: #INTERVAL,
        mandatory: true
      }
      FlightDate,

      @UI.lineItem: [{
      position: 70
      }]
      FlightPrice,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 10 }]
      _Customer.Street                            as Street,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 20 }]
      _Customer.PostalCode                        as PostalCode,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 30 }]
      _Customer.City                              as City,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 40 }]
      _Customer.CountryCode                       as Country,

      CurrencyCode,
      ConvertedFlightPrice,
      NewCurrencyCode,
      LocalLastChangedAt,

      /* Associations */
      _BookingSupplement,
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _CustomerName,
      _Status,
      _Travel,
      _BookSupp,

      // Criticality Column
      case
          when _Travel.Status = 'B' then 3
          else 0
      end                                         as StatusCriticality
}
```

#### Adjusting ZI_BookingSupplementName

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composite View for BookingSupplement'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MS_BookingSupplementName
  as select from ZI_MS_BookingSupplement
{
  key TravelId,
  key BookingId,
  key BookingSupplementId,

      @UI.lineItem: [{ position: 10, qualifier: 'AdditionalBookings' }]
      SupplementId,
      case
        when SupplementId like 'BV%' then 'Beverage'
        when SupplementId like 'ML%' then 'Meal'
        when SupplementId like 'LU%' then 'Luggage'
        when SupplementId like 'EX%' then 'Extras'
        else 'Unknown'
      end as SupplementDesc,

      @UI.lineItem: [{ position: 20, qualifier: 'AdditionalBookings' }]
      Price,
      CurrencyCode,
      LastChangedAt
}
```

### Adding further annotations

Changes:
- Added foreign key assocations to ZC_Booking and ZI_CustomerName
- Added text relationship to CustomerId with FullName

#### Adjusting ZC_Booking

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for BookingCurrency'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@UI.headerInfo: {
typeName: 'Booking',
    typeNamePlural: 'Bookings',
    title: {
        value: '_Customer.FullName',
        type: #STANDARD
    },
    description: {
        value: 'FlightDate',
        type: #STANDARD
    }

}

define view entity ZC_MS_Booking
  as select from ZI_MS_BookingCurrency(pDesiredCurrency: 'EUR')
  association [0..1] to ZC_MS_Customer              as _Customer on  $projection.CustomerID = _Customer.CustomerId
  association [0..*] to ZI_MS_BookingSupplementName as _BookSupp on  $projection.TravelID  = _BookSupp.TravelId
                                                                 and $projection.BookingID = _BookSupp.BookingId
{

      @UI.facet: [{
      type: #COLLECTION,
      id: 'CustomerCollection',
      label: 'Customer Information',
      purpose: #STANDARD,
      position: 10
      },{
      type: #FIELDGROUP_REFERENCE,
      id: 'GeneralCustomerData',
      targetQualifier: 'GeneralCustomerData',
      parentId: 'CustomerCollection',
      label: 'General Customer Data',
      position: 10
      },{
      type: #FIELDGROUP_REFERENCE,
      id: 'CustomerAddress',
      targetQualifier: 'CustomerAddress',
      parentId: 'CustomerCollection',
      label: 'Adresse',
      position: 20
      },{
      type: #LINEITEM_REFERENCE,
      id: 'AdditionalBookings',
      targetQualifier: 'AdditionalBookings',
      purpose: #STANDARD,
      label: 'Additional Bookings',
      targetElement: '_BookSupp',
      position: 30
      }]

      @UI.lineItem: [{
      position: 10,
      criticality: 'StatusCriticality'
      }]
  key TravelID,
  key BookingID,
      BookingDate,

      @UI.lineItem: [{
      position: 30
      }]
      @Consumption.valueHelpDefinition: [{
            entity: { name: 'ZC_MS_Customer', element: 'CustomerId'}
      }]
      @UI.selectionField: [{
        position: 20
       }]
      @UI.fieldGroup: [{ qualifier: 'GeneralCustomerData', position: 10 }]
      @ObjectModel.text.association: '_CustomerName'
      CustomerID,

      @UI.fieldGroup: [{ qualifier: 'GeneralCustomerData', position: 20 }]
      _Customer.FullName,

      @UI.lineItem: [{
      position: 40
      }]
      CarrierId,

      @UI.lineItem: [{
      position: 50
      }]
      _Connection.DepartureAirport                as DepartureAirport,

      @UI.lineItem: [{
      position: 60
      }]
      _Connection.DestinationAirport              as DestinationAirport,

      @UI.selectionField: [{
        position: 30
      }]
      @Consumption.valueHelpDefinition: [{
            entity: { name: 'ZMIND2E_I_Connection', element: 'Country'}
      }]
      _Connection._DestinationAirport.CountryCode as DestinationCountry,
      BookingStatus,
      ConnectionID,

      @UI.lineItem: [{
      position: 20
      }]
      @UI.selectionField: [{
        position: 10
       }]
      @Consumption.filter: {
        selectionType: #INTERVAL,
        mandatory: true
      }
      FlightDate,

      @UI.lineItem: [{
      position: 70
      }]
      FlightPrice,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 10 }]
      _Customer.Street                            as Street,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 20 }]
      _Customer.PostalCode                        as PostalCode,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 30 }]
      _Customer.City                              as City,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 40 }]
      _Customer.CountryCode                       as Country,

      CurrencyCode,
      ConvertedFlightPrice,
      NewCurrencyCode,
      LocalLastChangedAt,

      /* Associations */
      _BookingSupplement,
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _CustomerName,
      _Status,
      _Travel,
      _BookSupp,

      // Criticality Column
      case
          when _Travel.Status = 'B' then 3
          else 0
      end                                         as StatusCriticality
}
```
#### Adjusting ZI_CustomerName

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composite Interface View for Customer'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@ObjectModel.representativeKey: 'CustomerId'
define view entity ZI_MS_CustomerName
  as select from ZI_MS_Customer
{
      @ObjectModel.text.element: ['FullName']
  key CustomerId,
      FirstName,
      LastName,
      concat_with_space( FirstName, LastName, 1 ) as FullName,
      cast(
        case Title
            when 'Mr.'  then 'M'
            when 'Mrs.' then 'F'
            else ''
        end as zmind2_gender
      )                                           as Gender,
      Title,
      Street,
      PostalCode,
      City,
      CountryCode,
      PhoneNumber,
      EmailAddress,
      CreatedBy,
      CreatedAt,
      LastChangedBy,
      LastChangedAt,

      _Country
}
```

## Access Controls

Preconditions for this step:
- Have ZC_Booking defined
- Have ZI_CustomerName defined
- Have ZC_Customer defined

### Defining Access Controls

#### Defining ZI_CustomerName

```cds
@EndUserText.label: 'Access Control for CustomerName'
@MappingRole: true
define role ZI_MS_CUSTOMERNAME {
    grant select on ZI_MS_CustomerName
    where CountryCode = 'DE';               
}
```

#### Defining ZC_Customer

```cds
@EndUserText.label: 'Access Control for Customer'
@MappingRole: true
define role ZC_MS_CUSTOMER {
    grant select on ZC_MS_Customer inherit ZI_MS_CustomerName;
}
```

#### Defining ZC_Booking

```cds
@EndUserText.label: 'Access Control for Booking'
@MappingRole: true
define role ZC_MS_Booking {
    grant select on ZC_MS_Booking
    where _Customer.CountryCode = 'DE';       
}
```

## Hierarchies

Preconditions for this step:
- None

### Defining Hierarchies

#### Defining own functional location ZI_Location

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Hierarchytest'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MS_Location
  as select from I_FunctionalLocation
  association [0..1] to ZI_MS_Location as _ParentLocation on $projection.SuperiorFunctionalLocation = _ParentLocation.FunctionalLocation
{
  key FunctionalLocation,
      _FunctionalLocationText[1:Language=$session.system_language].FunctionalLocationName as FunctionalLocationName,
      SuperiorFunctionalLocation,
      _ParentLocation
}
```

#### Defining own hierarchy ZI_Hierarchy

```cds
define hierarchy ZI_MS_Hierarchy
  as parent child hierarchy(
    source ZI_MS_Location
    child to parent association _ParentLocation
    siblings order by
      FunctionalLocation ascending
    orphans root
  )
{
  FunctionalLocation,
  SuperiorFunctionalLocation,
  FunctionalLocationName,
  $node.parent_id             as ParentNode,
  $node.node_id               as NodeId,
  $node.hierarchy_is_cycle    as IsCycle,
  $node.hierarchy_is_orphan   as IsOrphan,
  $node.hierarchy_level       as HierarchyLevel,
  $node.hierarchy_rank        as HierarchyRank,
  $node.hierarchy_parent_rank as HierarchyParentRank,
  $node.hierarchy_tree_size   as HierarchyTreeSize,

  _ParentLocation

}
```

#### Testing the hierarchy with ZI_Hierarchy (Program)

```abap
*&---------------------------------------------------------------------*
*& Report zi_ms_hierarchy
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zi_ms_hierarchy.

TYPES:
  BEGIN OF ty_gs_hierarchy_display,
    FunctionalLocation         TYPE tplnr,
    FunctionalLocationName     TYPE pltxt,
    SuperiorFunctionalLocation TYPE tplma,
    node_id                    TYPE char30,
    parent_id                  TYPE char30,
    hierarchy_level            TYPE i,
    hierarchy_tree_size        TYPE i,
    hierarchy_rank             TYPE i,
    hierarchy_parent_rank      TYPE i,
    hierarchy_is_orphan        TYPE int1,
    hierarchy_is_cycle         TYPE int1,
    alv_node_key               TYPE salv_de_node_key,
  END OF ty_gs_hierarchy_display.
TYPES: ty_gt_hierarchy_display TYPE STANDARD TABLE OF ty_gs_hierarchy_display.

* read from hierarchy
DATA: gt_hierarchy_display  TYPE ty_gt_hierarchy_display.
SELECT FROM zi_ms_hierarchy
    FIELDS
      FunctionalLocation,
      FunctionalLocationName,
      SuperiorFunctionalLocation,
      NodeId AS node_id,
      ParentNode AS parent_id,
      IsCycle AS hierarchy_is_cycle,
      IsOrphan AS hierarchy_is_orphan,
      HierarchyLevel AS hierarchy_level,
      HierarchyTreeSize AS hierarchy_tree_size,
      HierarchyRank AS hierarchy_rank,
      HierarchyParentRank AS hierarchy_parent_rank
    ORDER BY hierarchy_rank
    INTO CORRESPONDING FIELDS OF TABLE @gt_hierarchy_display.

* display hierarchy in ALV tree
DATA: gr_tree                 TYPE REF TO cl_salv_tree.
DATA: gt_hierarchy_display2   TYPE ty_gt_hierarchy_display.
DATA: lr_alv_node             TYPE REF TO cl_salv_node.

* create empty tree
TRY.
    cl_salv_tree=>factory( IMPORTING  r_salv_tree = gr_tree
                           CHANGING   t_table     = gt_hierarchy_display2 ).
  CATCH cx_salv_error.
ENDTRY.

* add nodes to the tree
DATA(gr_alv_nodes) = gr_tree->get_nodes( ).
DATA: lv_tree_text TYPE lvc_value.
LOOP AT gt_hierarchy_display ASSIGNING FIELD-SYMBOL(<ls_node_data>).

  TRY.
      IF <ls_node_data>-hierarchy_level = 1.
        lv_tree_text = <ls_node_data>-FunctionalLocationName.
        lr_alv_node = gr_alv_nodes->add_node(
                                related_node = space
                                relationship = cl_gui_column_tree=>relat_last_child
                                text         = lv_tree_text
                            ).
        <ls_node_data>-alv_node_key = lr_alv_node->get_key( ).
        lr_alv_node->set_data_row(  <ls_node_data> ).
      ELSE.
        READ TABLE gt_hierarchy_display WITH KEY hierarchy_rank = <ls_node_data>-hierarchy_parent_rank
                                        ASSIGNING FIELD-SYMBOL(<ls_parent_data>).
        lv_tree_text = <ls_node_data>-FunctionalLocationName.
        lr_alv_node = gr_alv_nodes->add_node(
                                related_node = <ls_parent_data>-alv_node_key
                                relationship = cl_gui_column_tree=>relat_last_child
                                text         = lv_tree_text
                            ).
        <ls_node_data>-alv_node_key = lr_alv_node->get_key( ).
        lr_alv_node->set_data_row(  <ls_node_data> ).
      ENDIF.
    CATCH cx_salv_msg.
  ENDTRY.

ENDLOOP.

* set column width and labels
DATA(lr_columns) = gr_tree->get_columns( ).
lr_columns->set_optimize( 'X' ).

TRY.
    DATA: lr_column               TYPE REF TO cl_salv_column_tree.
    lr_column ?= lr_columns->get_column( 'FUNCTIONALLOCATIONNAME' ).
    lr_column->set_short_text('Name').
    lr_column->set_technical(  ).
    lr_column ?= lr_columns->get_column( 'NODE_ID' ).
    lr_column->set_short_text('NodeID').
    lr_column ?= lr_columns->get_column( 'PARENT_ID' ).
    lr_column->set_short_text('ParentID').
    lr_column ?= lr_columns->get_column( 'HIERARCHY_LEVEL' ).
    lr_column->set_short_text('Lvl').
    lr_column ?= lr_columns->get_column( 'HIERARCHY_TREE_SIZE' ).
    lr_column->set_short_text('TrSz').
    lr_column ?= lr_columns->get_column( 'HIERARCHY_RANK' ).
    lr_column->set_short_text('Rnk').
    lr_column ?= lr_columns->get_column( 'HIERARCHY_PARENT_RANK' ).
    lr_column->set_short_text('PaRnk').
    lr_column ?= lr_columns->get_column( 'HIERARCHY_IS_ORPHAN' ).
    lr_column->set_short_text('Orph').
    lr_column ?= lr_columns->get_column( 'HIERARCHY_IS_CYCLE' ).
    lr_column->set_short_text('Cycle').
    lr_column ?= lr_columns->get_column( 'FUNCTIONALLOCATION' ).
    lr_column->set_short_text('Location').
    lr_column ?= lr_columns->get_column( 'ALV_NODE_KEY' ).
    lr_column->set_short_text('ALVKey').
    lr_column->set_technical(  ).
  CATCH cx_salv_not_found.
ENDTRY.

* display the tree
gr_tree->display( ).
```

## Table functions

Preconditions for this step:
- Have ZC_Customer defined (Consumption View)
- Have ZI_BookingSupplementName defined (Composite View)

### Defining the table function

#### Defining ZTF_MS_BookingSupplement

```cds
@EndUserText.label: 'TF for comma separated strings'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@ClientHandling.type: #CLIENT_INDEPENDENT
define table function ZTF_MS_BookingSupplement
returns
{
  key travel_id   : zmind2_travel_id;
  key booking_id  : zmind2_booking_id;
      supplements : abap.string;
}
implemented by method
  ZCL_MS_BOOKING_SUPPLEMENT=>GET_SUPPLEMENTS;
```

### Implementing AMDP

#### Implementing TF Method get_supplements in ZCL_Booking_Supplement

```abap
CLASS zcl_ms_booking_supplement DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_amdp_marker_hdb.
    CLASS-METHODS get_supplements FOR TABLE FUNCTION ZTF_MS_BookingSupplement.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_ms_booking_supplement IMPLEMENTATION.
  METHOD get_supplements BY DATABASE FUNCTION
                                      FOR HDB
                                      LANGUAGE SQLSCRIPT
                                      OPTIONS READ-ONLY
                                      USING zmind2_book_supp.

    RETURN SELECT travel_id,
                  booking_id,
                  STRING_AGG(supplement_id, ', ') AS supplements
                FROM zmind2_book_supp
                GROUP BY travel_id, booking_id;

ENDMETHOD.
ENDCLASS.
```

### Defining a composite view of the table function

#### Defining ZI_MS_BookingSupplementsTF

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Composite View for BookSuppTF'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_MS_BookingSupplementsTF
  as select from ZTF_MS_BookingSupplement
{
  key travel_id,
  key booking_id,
      supplements
}
```

Changes:
- Added the concatenated supplements to the booking consumption view and added an annotation

### Adjusting ZC_Booking

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View for BookingCurrency'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@UI.headerInfo: {
typeName: 'Booking',
    typeNamePlural: 'Bookings',
    title: {
        value: '_Customer.FullName',
        type: #STANDARD
    },
    description: {
        value: 'FlightDate',
        type: #STANDARD
    }

}

define view entity ZC_MS_Booking
  as select from ZI_MS_BookingCurrency(pDesiredCurrency: 'EUR')
  association [0..1] to ZC_MS_Customer              as _Customer                on  $projection.CustomerID = _Customer.CustomerId
  association [0..*] to ZI_MS_BookingSupplementName as _BookSupp                on  $projection.TravelID  = _BookSupp.TravelId
                                                                                and $projection.BookingID = _BookSupp.BookingId
  association [1..1] to ZI_MS_BookingSupplementsTF  as _ConcatenatedSupplements on  $projection.TravelID  = _ConcatenatedSupplements.travel_id
                                                                                and $projection.BookingID = _ConcatenatedSupplements.booking_id
{

      @UI.facet: [{
      type: #COLLECTION,
      id: 'CustomerCollection',
      label: 'Customer Information',
      purpose: #STANDARD,
      position: 10
      },{
      type: #FIELDGROUP_REFERENCE,
      id: 'GeneralCustomerData',
      targetQualifier: 'GeneralCustomerData',
      parentId: 'CustomerCollection',
      label: 'General Customer Data',
      position: 10
      },{
      type: #FIELDGROUP_REFERENCE,
      id: 'CustomerAddress',
      targetQualifier: 'CustomerAddress',
      parentId: 'CustomerCollection',
      label: 'Adresse',
      position: 20
      },{
      type: #LINEITEM_REFERENCE,
      id: 'AdditionalBookings',
      targetQualifier: 'AdditionalBookings',
      purpose: #STANDARD,
      label: 'Additional Bookings',
      targetElement: '_BookSupp',
      position: 30
      }]

      @UI.lineItem: [{
      position: 10,
      criticality: 'StatusCriticality'
      }]
  key TravelID,
  key BookingID,
      BookingDate,

      @UI.lineItem: [{
      position: 30
      }]
      @Consumption.valueHelpDefinition: [{
            entity: { name: 'ZC_MS_Customer', element: 'CustomerId'}
      }]
      @UI.selectionField: [{
        position: 20
       }]
      @UI.fieldGroup: [{ qualifier: 'GeneralCustomerData', position: 10 }]
      @ObjectModel.text.association: '_CustomerName'
      CustomerID,

      @UI.fieldGroup: [{ qualifier: 'GeneralCustomerData', position: 20 }]
      _Customer.FullName,

      @UI.lineItem: [{ position: 80 }]
      _ConcatenatedSupplements.supplements        as AllSupplements,

      @UI.lineItem: [{
      position: 40
      }]
      CarrierId,

      @UI.lineItem: [{
      position: 50
      }]
      _Connection.DepartureAirport                as DepartureAirport,

      @UI.lineItem: [{
      position: 60
      }]
      _Connection.DestinationAirport              as DestinationAirport,

      @UI.selectionField: [{
        position: 30
      }]
      @Consumption.valueHelpDefinition: [{
            entity: { name: 'ZMIND2E_I_Connection', element: 'Country'}
      }]
      _Connection._DestinationAirport.CountryCode as DestinationCountry,
      BookingStatus,
      ConnectionID,

      @UI.lineItem: [{
      position: 20
      }]
      @UI.selectionField: [{
        position: 10
       }]
      @Consumption.filter: {
        selectionType: #INTERVAL,
        mandatory: true
      }
      FlightDate,

      @UI.lineItem: [{
      position: 70
      }]
      FlightPrice,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 10 }]
      _Customer.Street                            as Street,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 20 }]
      _Customer.PostalCode                        as PostalCode,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 30 }]
      _Customer.City                              as City,

      @UI.fieldGroup: [{ qualifier: 'CustomerAddress', position: 40 }]
      _Customer.CountryCode                       as Country,

      CurrencyCode,
      ConvertedFlightPrice,
      NewCurrencyCode,
      LocalLastChangedAt,

      /* Associations */
      _BookingSupplement,
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _CustomerName,
      _Status,
      _Travel,
      _BookSupp,

      // Criticality Column
      case
          when _Travel.Status = 'B' then 3
          else 0
      end                                         as StatusCriticality
}
```

## Cubes and Analytics

### Defining a cube

#### Defining Z_BookingCube

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@Analytics.dataCategory: #CUBE
@EndUserText.label: 'Cube for Booking'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZMS_BookingCube
  as select from ZI_MS_BookingCurrency(pDesiredCurrency:'EUR')
  association [0..1] to I_CalendarDate              as _Calendar   on  $projection.FlightDate = _Calendar.CalendarDate
  association [0..*] to ZI_MS_BookingSupplementName as _Supplement on  $projection.TravelID  = _Supplement.TravelId
                                                                   and $projection.BookingID = _Supplement.BookingId
{
  key TravelID,
  key BookingID,
      _CustomerName.FullName,
      BookingDate,
      CustomerID,
      CarrierId,
      BookingStatus,
      ConnectionID,
      FlightDate,
      FlightPrice,
      CurrencyCode,
      ConvertedFlightPrice,
      NewCurrencyCode,
      _Calendar.CalendarYear,
      _Calendar.CalendarQuarter,
      _Calendar.CalendarMonth,
      LocalLastChangedAt,

      @Aggregation.default: #MIN
      @Semantics.amount.currencyCode: 'NewCurrencyCode'
      min(
      case
        when _Supplement.SupplementDesc = 'Meal' then _Supplement.Price
        else null
      end
      ) as MinPriceMeal,

      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'NewCurrencyCode'
      sum(
      case
        when _Supplement.SupplementDesc = 'Meal' then _Supplement.Price
        else null
      end
      ) as PriceSumMeal,

      /* Associations */
      _BookingSupplement,
      _Carrier,
      _Connection,
      _Currency,
      _Customer,
      _CustomerName,
      _Status,
      _Travel,
      _Calendar,
      _Supplement
}

group by
  TravelID,
  BookingID,
  _CustomerName.FullName,
  BookingDate,
  CustomerID,
  CarrierId,
  BookingStatus,
  ConnectionID,
  FlightDate,
  FlightPrice,
  CurrencyCode,
  ConvertedFlightPrice,
  NewCurrencyCode,
  _Calendar.CalendarYear,
  _Calendar.CalendarQuarter,
  _Calendar.CalendarMonth,
  LocalLastChangedAt
```

### Defining a query

#### Defining Z_C_BookingQuery

```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Query for Booking Cube'
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@Analytics.query: true
define view entity ZMS_C_BookingQuery
  as select from ZMS_BookingCube
{
  key CarrierId,
  key CalendarYear,
  key CalendarQuarter,
      MinPriceMeal,
      PriceSumMeal,
      NewCurrencyCode
}
group by
  CarrierId,
  CalendarYear,
  CalendarQuarter,
  MinPriceMeal,
  PriceSumMeal,
  NewCurrencyCode
```

## Custom Entities
Preconditions for this step:
- None

### Defining the custom entity
```cds
@EndUserText.label: 'Custom Entity View Example'
@ObjectModel.query.implementedBy: 'ABAP:ZCL_MS_CUSTOM_ENTITY'
define custom entity ZI_MS_CustomEntity
{
      @UI.lineItem:[ { position: 10 } ]
  key TravelId   : abap.numc( 8 );
      @UI.lineItem:[ { position: 20 } ]
      AgencyId   : abap.numc( 6 );
      @UI.lineItem:[ { position: 30 } ]
      CustomerId : abap.numc( 6 );
      @UI.lineItem:[ { position: 40 } ]
      BeginDate  : abap.dats;
      @UI.lineItem:[ { position: 50 } ]
      EndDate    : abap.dats;
      @UI.lineItem:[ { position: 60 } ]
      FullPrice  : abap.dec( 17, 3 );
      @UI.lineItem:[ { position: 70 } ]
      Currency   : abap.cuky;
      @UI.lineItem:[ { position: 80 } ]
      Status     : abap.char( 1 );
}
```

### Defining the implementation class
```abap
CLASS zcl_ms_custom_entity DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zcl_ms_custom_entity IMPLEMENTATION.

  METHOD if_rap_query_provider~select.
    DATA dummy_data TYPE STANDARD TABLE OF ZI_DM_CustomEntity.

    IF io_request->is_data_requested(  ).
      " TODO: Use following fields to return only requested data
      DATA(lv_offset) = io_request->get_paging( )->get_offset( ).
      DATA(lv_page_size) = io_request->get_paging( )->get_page_size( ).
      DATA(lv_max_rows) = COND #( WHEN lv_page_size = if_rap_query_paging=>page_size_unlimited
                                THEN 0 ELSE lv_page_size ).

      dummy_data = VALUE #(
          ( TravelId = 1 AgencyId = 1 CustomerId = 1 BeginDate = '20210901' EndDate = '20211009' FullPrice = 1500 Currency = 'EUR' Status = 'A')
          ( TravelId = 2 AgencyId = 2 CustomerId = 4 BeginDate = '20230603' EndDate = '20230703' FullPrice = 1800 Currency = 'EUR' Status = 'A')
          ( TravelId = 3 AgencyId = 1 CustomerId = 2 BeginDate = '20220201' EndDate = '20220309' FullPrice = 1200 Currency = 'USD' Status = 'A') ).

      io_response->set_data( dummy_data ).
    ENDIF.

    IF io_request->is_total_numb_of_rec_requested(  ).
      io_response->set_total_number_of_records( iv_total_number_of_records = 5 ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
```

## Use of CDS Views in classic ABAP Code

Preconditions for this step:
- Have ZI_CustomerName defined (Composite Interface View)

### Program Z_CDS_ABAPSQL

```abap
*&---------------------------------------------------------------------*
*& Report zms_cds_abapsql
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zms_cds_abapsql.

SELECT FROM zc_ms_customername
  FIELDS CustomerId, FullName
  INTO TABLE @DATA(customername).

cl_demo_output=>display( customername ).
```

Preconditions for this step:
- Have ZC_CustomerName defined (Consumption View)

### Program Z_CDS_ABAPSQL_ALV
```abap
*&---------------------------------------------------------------------*
*& Report zms_cds_abapsql_alv
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zms_cds_abapsql_alv.

DATA: customer TYPE ZC_MS_CustomerName.

SELECT-OPTIONS: country FOR customer-CountryCode,
                gender FOR customer-Gender.

END-OF-SELECTION.

  DATA(alv) = cl_salv_gui_table_ida=>create_for_cds_view(
        iv_cds_view_name = 'ZC_MS_CustomerName' ).

  DATA(collector) = NEW cl_salv_range_tab_collector( ).

  collector->add_ranges_for_name(
    iv_name   = 'COUNTRYCODE'
    it_ranges = country[]
  ).

  collector->add_ranges_for_name(
      iv_name = 'GENDER'
      it_ranges = gender[] ).

  collector->get_collected_ranges(
    IMPORTING
      et_named_ranges = DATA(named_ranges)
  ).

  alv->set_select_options(
      it_ranges = named_ranges ).

  alv->fullscreen( )->display( ).
```

## Unit Tests

Preconditions for this step:
- Have ZI_CustomerName defined (Composite Interface View)

### Unit Test CustomerName - FullName and Gender

```abap
"!@testing ZI_MS_CustomerName
CLASS ltc_ZMS_I_CustomerName
DEFINITION FINAL FOR TESTING
DURATION SHORT
RISK LEVEL HARMLESS.
  PRIVATE SECTION.

    CLASS-DATA:
      environment TYPE REF TO if_cds_test_environment.

    CLASS-METHODS:
      "! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.
      class_setup RAISING cx_static_check,
      "! In CLASS_TEARDOWN, Generated database entities (doubles & clones) should be deleted at the end of test class execution.
      class_teardown.

    DATA:
      act_results         TYPE STANDARD TABLE OF zi_ms_customername WITH EMPTY KEY,
      lt_zalst_i_customer TYPE STANDARD TABLE OF zi_ms_customer WITH EMPTY KEY,
      test_data           TYPE REF TO if_cds_test_data.

    METHODS:
      "! SETUP method creates a common start state for each test method,
      "! clear_doubles clears the test data for all the doubles used in the test method before each test method execution.
      setup RAISING cx_static_check,
      prepare_testdata_set,
      "!  In this method test data is inserted into the generated double(s) and the test is executed and
      "!  the results should be asserted with the actuals.
      FalseFullName_test FOR TESTING RAISING cx_static_check,
      FalseGender_test FOR TESTING RAISING cx_static_check,
      RightFullName_test FOR TESTING RAISING cx_static_check,
      RightGender_test FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_ZMS_I_CustomerName IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_cds_test_environment=>create( i_for_entity = 'ZI_MS_CustomerName' ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD FalseFullName_test.
    prepare_testdata_set( ).

    DATA: lv_expected TYPE STANDARD TABLE OF zi_ms_customername.

    lv_expected = VALUE #( (
          FullName = 'Theresia Buchhol,'
      ) ).
    SELECT * FROM zi_ms_customername INTO TABLE @act_results.
    cl_abap_unit_assert=>assert_differs(
      EXPORTING
        act = act_results[ 1 ]-FullName
        exp = lv_expected[ 1 ]-FullName
        msg = 'The FullName field is unexpectedly equal to the expected value.'
    ).
  ENDMETHOD.

    METHOD RightFullName_test.
    prepare_testdata_set( ).

    DATA: lv_expected TYPE STANDARD TABLE OF zi_ms_customername.

    lv_expected = VALUE #( (
          FullName = 'Theresia Buchholm'
      ) ).
    SELECT * FROM zi_ms_customername INTO TABLE @act_results.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = act_results[ 1 ]-FullName
        exp = lv_expected[ 1 ]-FullName
        msg = 'The FullName field is expected to be equal to the expected value.'
    ).
  ENDMETHOD.

  METHOD prepare_testdata_set.

    lt_zms_i_customer = VALUE #(
      (
            Customer ='000001'
            FirstName ='Theresia'
            LastName ='Buchholm'
            Title ='Mrs.'
            Street ='Lerchenstr. 82'
            PostalCode ='71116'
            City ='Gaertringen'
            CountryCode ='DE'
            PhoneNumber ='+49-341-184709'
            EmailAddress ='theresia.buchholm@flight.example.de'
            CreatedBy =''
            CreateDate ='0.0000000 '
            LastChangedBy =''
            LastChangeDate ='0.0000000 '
      ) ).
    environment->insert_test_data( i_data =  lt_zalst_i_customer ).

  ENDMETHOD.

  METHOD FalseGender_test.

    prepare_testdata_set( ).

    DATA: lv_expected TYPE STANDARD TABLE OF zi_ms_customername.
    lv_expected = VALUE #(
      (
            Title = 'M'
      ) ).
    environment->insert_test_data( i_data = lt_zms_i_customer ).

    SELECT * FROM zi_ms_customername INTO TABLE @act_results.
    cl_abap_unit_assert=>assert_differs(
      EXPORTING
        act = act_results[ 1 ]-FullName
        exp = lv_expected[ 1 ]-FullName
        msg = 'The gender field is unexpectedly equal to the expected value.'
    ).

  ENDMETHOD.

    METHOD RightGender_test.

    prepare_testdata_set( ).

    DATA: lv_expected TYPE STANDARD TABLE OF zi_ms_customername.
    lv_expected = VALUE #(
      (
            Title = 'F'
      ) ).
    environment->insert_test_data( i_data = lt_zms_i_customer ).

    SELECT * FROM zi_ms_customername INTO TABLE @act_results.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = act_results[ 1 ]-FullName
        exp = lv_expected[ 1 ]-FullName
        msg = 'The gender field is expected to be equal to the expected value.'
    ).

  ENDMETHOD.

ENDCLASS.

```


### Unit Test parameters and conversions

Preconditions for this step:
- Have ZI_BookingCurrency defined (Composite Interface View)

TODO: Die Währungsumrechnung funktioniert noch nicht!!!

```abap
"!@testing ZI_MS_BookingCurrency
CLASS ltc_ZI_MS_BookingCurrency
DEFINITION FINAL FOR TESTING
DURATION SHORT
RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    CLASS-DATA environment TYPE REF TO if_cds_test_environment.

    "! In CLASS_SETUP, corresponding doubles and clone(s) for the CDS view under test and its dependencies are created.
    "!
    "! @raising cx_static_check  |
    CLASS-METHODS class_setup RAISING cx_static_check.
    "! In CLASS_TEARDOWN, Generated database entities (doubles & clones) should be deleted at the end of test class execution.
    CLASS-METHODS class_teardown.

    DATA act_results          TYPE STANDARD TABLE OF zi_ms_bookingcurrency WITH EMPTY KEY.
    DATA lt_zmind2e_i_booking TYPE STANDARD TABLE OF zmind2e_i_booking WITH EMPTY KEY.
    data lt_customer_name     type standard table of zi_ms_customername with empty key.

    "! SETUP method creates a common start state for each test method,
    "! clear_doubles clears the test data for all the doubles used in the test method before each test method execution.
    "!
    "! @raising cx_static_check |
    METHODS setup RAISING cx_static_check.
    METHODS prepare_testdata_set.
    "!  In this method test data is inserted into the generated double(s) and the test is executed and
    "!  the results should be asserted with the actuals.
    METHODS check_correct_processing  FOR TESTING RAISING cx_static_check.
    METHODS check_correct_assoc FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_ZALST_I_BOOKINGCURRENCY IMPLEMENTATION.

  METHOD class_setup.
    environment = cl_cds_test_environment=>create( i_for_entity = 'ZALST_I_BOOKINGCURRENCY' test_associations = 'X' ).
  ENDMETHOD.

  METHOD setup.
    environment->clear_doubles( ).
  ENDMETHOD.

  METHOD class_teardown.
    environment->destroy( ).
  ENDMETHOD.

  METHOD check_correct_processing.
    prepare_testdata_set( ).
    DATA: lt_expected TYPE STANDARD TABLE OF zi_ms_bookingcurrency.

    lt_expected = VALUE #( (
        FlightDate = '20230323'
        FlightPrice = '438.00'
        OldCurrencyCode = 'USD'
        ConvertedFlightPrice = '413.21'
        NewCurrencyCode = 'EUR'
      ) ).
    SELECT * FROM  zi_ms_bookingcurrency( DesiredCurrency = 'EUR' ) INTO TABLE @act_results.
    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act                  = act_results
        exp                  = lt_expected
        msg                  = 'The currency was not converted correctly.'
    ).
  ENDMETHOD.

  METHOD prepare_testdata_set.

    "Prepare test data for 'zmind2e_i_booking'
    "TODO: Provide the test data here
    lt_zmind2e_i_booking = VALUE #(
      (
        FlightDate = '20230915'
        FlightPrice = '500.00'
        CurrencyCode = 'USD'
        CustomerID = '123'
      ) ).
    environment->insert_test_data( i_data =  lt_zmind2e_i_booking ).

    DATA(test_data) = cl_cds_test_data=>create_currency_conv_data( output = '550.00' )->for_parameters(
                                                                                 amount             = '500.00'
                                                                                 source_currency    = 'USD'
                                                                                 target_currency    = 'EUR'
                                                                                 exchange_rate_date = '20230915'
                                                                               ).

    DATA(curr_conv_data_stub) = environment->get_double( cl_cds_test_environment=>currency_conversion ).
    curr_conv_data_stub->insert( i_test_data = test_data ).

    lt_customer_name = VALUE #(
      ( Customer = '123' FullName = 'John Doe' ) ). " Example customer data

    environment->insert_test_data( i_data = lt_customer_name ).

  ENDMETHOD.

  METHOD check_correct_assoc.
    prepare_testdata_set( ).

    DATA lv_expected TYPE TABLE OF zi_ms_bookingcurrency.
    lv_expected = VALUE #( ( FullName = 'John Doe' ) ).

    SELECT *
      FROM zi_ms_bookingcurrency( DesiredCurrency = 'EUR' )
      INTO TABLE @DATA(act_results).

    LOOP AT act_results INTO DATA(act_result).
      cl_abap_unit_assert=>assert_equals( exp = lv_expected[ 1 ]-FullName
                                          act = act_result-FullName
                                          msg = ‘Association to zi_ms_customername is empty’ ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

```

### Unit Test link views

TODO: Hinzufügen!!!

### Unit Test Access Control

TODO: Hinzufügen!!!
