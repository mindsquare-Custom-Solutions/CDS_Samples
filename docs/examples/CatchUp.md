# Calendar Week 33
## Exercise 1 - Create your first view
```cds
@AbapCatalog.viewEnhancementCategory: [#PROJECTION_LIST]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Basic View Entity'
define view entity ZI_<YOUR_NAME_ABBREVIATION>_Customer
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

# Calendar Week 35
## Exercise 2 - Annotations
```cds
@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Booking Basic View Entity'

define view entity ZI_<YOUR_NAME_ABBREVIATION>_Booking
  as select from zmind2_booking
{
  key travel_id             as TravelID,
  key booking_id            as BookingID,
      booking_date          as BookingDate,
      customer_id           as CustomerID,
      carrier_id            as CarrierId,
      connection_id         as ConnectionID,
      flight_date           as FlightDate,

    	@Semantics.amount.currencyCode: 'CurrencyCode'
      flight_price          as FlightPrice,
      currency_code         as CurrencyCode,

      @EndUserText.label: 'Last changed date'
      local_last_changed_at as LocalLastChangedAt
}
```

# Calendar Week 10/2024

## Fiori Object Page

ZC_JR2_TravelTP
```cds
@Metadata.layer: #CORE

@Search.searchable: true

@UI.headerInfo: {
  typeName: 'Travel',
  typeNamePlural: 'Travels',
  title: {
    value: 'TravelId',
    type: #STANDARD
  },
  description: {
    value: 'Description',
    type: #STANDARD
  }
}
annotate view ZC_JR2_TravelTP with
{

  @UI.facet: [
    {
      id: 'Travel',
      purpose: #STANDARD,
      type: #COLLECTION,
      label: 'Travel',
      position: 10
    },
    {
      id: 'Bookings',
      purpose: #STANDARD,
      type: #LINEITEM_REFERENCE,
      targetQualifier: 'Booking',
      label: 'Bookings',
      position: 20,
      targetElement: '_Bookings'
    },
    {
      id: 'Overview',
      type: #FIELDGROUP_REFERENCE,
      label: 'Overview',
      parentId: 'Travel',
      targetQualifier: 'Overview',
      position: 10
    },
    {
      id: 'Dates',
      type: #FIELDGROUP_REFERENCE,
      label: 'Dates',
      parentId: 'Travel',
      targetQualifier: 'Dates',
      position: 20
    },
    {
      id: 'Price',
      type: #FIELDGROUP_REFERENCE,
      label: 'Price',
      parentId: 'Travel',
      targetQualifier: 'Price',
      position: 30
    }
  ]
  

  @UI.lineItem: [{ position: 10 }]
  @UI.fieldGroup: [{ qualifier: 'Overview', position: 10 }]
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.2
  @Search.ranking: #HIGH
  TravelId;

  @UI.fieldGroup: [{ qualifier: 'Overview', position: 20 }]
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.2
  @Search.ranking: #HIGH
  AgencyId;

  @UI.lineItem: [{ position: 20 }]
  @UI.fieldGroup: [{ qualifier: 'Overview', position: 40 }]
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.5
  @Search.ranking: #MEDIUM
  Description;

  @UI.lineItem: [{ position: 40 }]
  @UI.selectionField: [{ position: 20 }]
  @UI.fieldGroup: [{ qualifier: 'Overview', position: 30 }]
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.2
  @Search.ranking: #MEDIUM
  @Consumption.valueHelpDefinition: [{
    entity: {
      name: 'ZMIND2E_I_Customer',
      element: 'CustomerId'
    }
  }]
  CustomerId;

  @UI.lineItem: [{ position: 50 }]
  @UI.selectionField: [{ position: 10 }]
  @UI.fieldGroup: [{ qualifier: 'Dates', position: 10 }]
  @Consumption.filter: {
    selectionType: #INTERVAL,
    mandatory: false
  }
  BeginDate;

  @UI.lineItem: [{ position: 60 }]
  @UI.fieldGroup: [{ qualifier: 'Dates', position: 20 }]
  EndDate;

  @UI.lineItem: [{ position: 80 }]
  @UI.fieldGroup: [{ qualifier: 'Price', position: 10 }]
  TotalPrice;
  
  @UI.fieldGroup: [{ qualifier: 'Price', position: 20 }]
  CurrencyCode;
  
  @UI.fieldGroup: [{ qualifier: 'Price', position: 30 }]
  BookingFee;

  @UI.selectionField: [{ position: 30 }]
  @UI.fieldGroup: [{ qualifier: 'Overview', position: 50 }]
  Status;
}
```

ZC_JR2_BookingTP
```cds
@Metadata.layer: #CORE

@UI.headerInfo: {
    typeName: 'Booking',
    typeNamePlural: 'Bookings',
    title.value: 'BookingDate',
    description.value: 'CustomerId'
}

@UI.presentationVariant: [{ 
    sortOrder: [{ by: 'FlightDate', direction: #ASC }]
}]

annotate view ZC_JR2_BookingTP
  with 
{
  @UI.facet: [{
      type: #COLLECTION,
      id: 'DetailsCollection',
      purpose: #STANDARD,
      label: 'Details',
      position: 10
  },{
      type: #FIELDGROUP_REFERENCE,
      id: 'BookingFieldGroup',
      targetQualifier: 'Booking',
      parentId: 'DetailsCollection',
      label: 'Booking',
      position: 10
  },{
      type: #FIELDGROUP_REFERENCE,
      id: 'FlightFieldGroup',
      targetQualifier: 'Flight',
      parentId: 'DetailsCollection',
      label: 'Flight',
      position: 20
  }]


  @UI.lineItem: [{ qualifier: 'Booking', position: 10 }]
  @UI.fieldGroup: [{ qualifier: 'Booking', position: 10 }]
  BookingId;

  @UI.lineItem: [{ qualifier: 'Booking', position: 15 }]
  @UI.fieldGroup: [{ qualifier: 'Booking', position: 15 }]
  BookingStatus;

  @UI.lineItem: [{ qualifier: 'Booking', position: 20 }]
  @UI.fieldGroup: [{ qualifier: 'Booking', position: 20 }]
  BookingDate;

  @UI.lineItem: [{ qualifier: 'Booking', position: 30 }]
  @UI.fieldGroup: [{ qualifier: 'Flight', position: 30 }]
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'ZMIND2E_I_FlightVH', element: 'CarrierId' },
    additionalBinding: [
      { element: 'ConnectionID', localElement: 'ConnectionId', usage: #RESULT },
      { element: 'FlightDate', localElement: 'FlightDate', usage: #RESULT }
    ]
  }]
  CarrierId;

  @UI.lineItem: [{ qualifier: 'Booking', position: 40 }]
  @UI.fieldGroup: [{ qualifier: 'Flight', position: 40 }]
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'ZMIND2E_I_FlightVH', element: 'ConnectionID' },
    additionalBinding: [
      { element: 'CarrierId', localElement: 'CarrierId', usage: #FILTER_AND_RESULT },
      { element: 'FlightDate', localElement: 'FlightDate', usage: #RESULT }
    ]
  }]
  ConnectionId;

  @UI.lineItem: [{ qualifier: 'Booking', position: 50 }]
  @UI.fieldGroup: [{ qualifier: 'Flight', position: 50 }]
  @Consumption.valueHelpDefinition: [{
    entity: { name: 'ZMIND2E_I_FlightVH', element: 'FlightDate' },
    additionalBinding: [
      { element: 'CarrierId', localElement: 'CarrierId', usage: #FILTER_AND_RESULT },
      { element: 'ConnectionID', localElement: 'ConnectionId', usage: #FILTER_AND_RESULT }
    ]
  }]
  FlightDate;

  @UI.lineItem: [{ qualifier: 'Booking', position: 60 }]
  @UI.fieldGroup: [{ qualifier: 'Flight', position: 60 }]
  FlightPrice;

  @Consumption.valueHelpDefinition: [{ entity.name: 'I_CurrencyStdVH', entity.element: 'Currency' }]
  CurrencyCode;

  @UI.fieldGroup: [{ qualifier: 'Booking', position: 70 }]
  LocalLastChangedAt;
}
```
