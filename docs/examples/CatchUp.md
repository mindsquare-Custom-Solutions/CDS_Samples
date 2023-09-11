# Calendar Week 33
## Exercise 1 - Create your first view
```cds
@AbapCatalog.viewEnhancementCategory: [#PROJECTION_LIST]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Customer Basic View Entity'
define view entity ZI<YOUR_NAME_ABBREVIATION>_Customer
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

define view entity ZMIND2E_I_Booking
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
      local_last_changed_at as LocalLastChangedAt
}
```
