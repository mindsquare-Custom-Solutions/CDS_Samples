## Codebeispiele für die Übung zum Flexible Programming Model

### Fragment

Code des Fragments in `FlightGeoMap.fragment.xml`

```xml
<core:FragmentDefinition 
	xmlns:core="sap.ui.core" 
	xmlns="sap.m" 
	xmlns:macros="sap.fe.macros">

  <mvc:View
  	id="zView"
  	xmlns:core="sap.ui.core" 
  	xmlns:mvc="sap.ui.core.mvc" 
  	xmlns="sap.m"
  	xmlns:html="http://www.w3.org/1999/xhtml" 
  	xmlns:l="sap.ui.layout"
  	xmlns:vbm="sap.ui.vbm">
  	
  	<l:VerticalLayout id="verticalLayout" width="100%">
  
  		<vbm:GeoMap 
  			id="GeoMapControl" 
  			width="100%" 
  			height="400px"
  			centerPosition="{_Connection/_DepartureAirport/Longitude};{_Connection/_DepartureAirport/Latitude}"
  			zoomlevel="3"
  			core:require="{ handler: 'zmind2/travel/ext/fragment/FlightGeoMap'}" 
  			>
  
  			<vbm:vos>
  				<vbm:Routes id="GeoMapRoutes">
  					<vbm:Route id="GeoMapRoute1" 
  						position="{_Connection/_DepartureAirport/Longitude};{_Connection/_DepartureAirport/Latitude};0; {_Connection/_DestinationAirport/Longitude};{_Connection/_DestinationAirport/Latitude};0"
  						color="rgb(92,186,230)" 
  						colorBorder="rgb(255,255,255)" 
  						linewidth="5"
  						routetype="Geodesic" />
  				</vbm:Routes>
  				<vbm:Spots id="GeoMapSpots" 
  					posChangeable="true"
  					scaleChangeable="true">
  					<vbm:items>
  						<vbm:Spot id="DepartureSpot" 
  							position="{_Connection/_DepartureAirport/Longitude};{_Connection/_DepartureAirport/Latitude};0" 
  							type="Default" 
  							text="{_Connection/_DepartureAirport/AirportId}" 
  							contentOffset="0;0" />
  						<vbm:Spot id="DestinationSpot" 
  							position="{_Connection/_DestinationAirport/Longitude};{_Connection/_DestinationAirport/Latitude};0" 
  							type="Success" 
  							text="{_Connection/_DestinationAirport/AirportId}" 
  							contentOffset="0;0" />
  					</vbm:items>
  				</vbm:Spots>
  			</vbm:vos>
  
  		</vbm:GeoMap>
  	</l:VerticalLayout>
  </mvc:View>
</core:FragmentDefinition>
```

### Controller Extension

Code der Controller Extension `MapController.controller.js`

```js
sap.ui.define(['sap/ui/core/mvc/ControllerExtension'], function (ControllerExtension) {
	'use strict';

	return ControllerExtension.extend('zmind2.travel.ext.controller.MapController', {
		// this section allows to extend lifecycle hooks or hooks provided by Fiori elements
		override: {
			/**
             * Called when a controller is instantiated and its View controls (if available) are already created.
             * Can be used to modify the View before it is displayed, to bind event handlers and do other one-time initialization.
             * @memberOf zmind2.travel.ext.controller.MapController
             */
			onInit: function () {
				// you can access the Fiori elements extensionAPI via this.base.getExtensionAPI
				var oModel = this.base.getExtensionAPI().getModel();
			},
			onAfterRendering: function (oObjectPageEvent) {

				//Get Extension API
				var oObjectPageController = this;
				var oExtensionAPI = oObjectPageController.base.getExtensionAPI();

				//Get Geo Map Controller. Full id: "zgeomovingobjectlistv4::MovingObjectObjectPage--fe::CustomSubSection::Geomap--GeoMapControl"
				var oGeomapController = oExtensionAPI.byId("fe::CustomSubSection::FlightGeoMap--GeoMapControl");

				//Set Map configuration
				var oMapConfig = {
					"MapProvider": [{
						"name": "OSM",
						"type": "",
						"description": "",
						"tileX": "256",
						"tileY": "256",
						"maxLOD": "20",
						"copyright": "OpenStreetMap",
						"Source": [{
							"id": "s1",
							"url": "https://a.tile.openstreetmap.org/{LOD}/{X}/{Y}.png"
						}]
					}],
					"MapLayerStacks": [{
						"name": "DEFAULT",
						"MapLayer": [{
							"name": "OSMLayter",
							"refMapProvider": "OSM",
							"opacity": "1.0",
							"colBkgnd": "RGB(255,255,255)"
						}]
					}]
				};

				oGeomapController.setMapConfiguration(oMapConfig);
				oGeomapController.setRefMapLayerStack("DEFAULT");
			}
		}
	});
});
```
