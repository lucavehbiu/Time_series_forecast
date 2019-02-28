# Optimizing energy consumption - 'Smart-home' perspective 

With the rise of IoT, the precision and the real-time feature of the data that is retrieved, in this case submeters can really make **a dent in the energy usage behavior of a household**. 

Machine learning algorithms for time series _(Fourier,  Holt-Winters and Arima)_ were implemented and the results which have become interactive through a **Shiny** application teach users how to _**consume less** and **more efficiently** (less energy losses)_. 

Predictions of future energy consumption and usage can be used from the home-builders to: 

- Better design their houses (thermo-efficient) 
- More knowledge to the household as to which appliance spends the most for e.g. 

Moreover, such predictions can be sent to the energy suppliers which in turn can predict their energy demand more accurately. By doing so, they can make up to households for the data gifted to them, by providing accurate prices for the next _48 hours_. Such symbiotic relationship between _home-builder, energy supplier and household_ is what will happen frequently in the future, which we more commonly know as **Smart-grid Cities**.

_Iot - Real benefits get visible as scale increases!_


#### File explanation:

- model.R -> code for modeling the time series and forecasting
- viz.R -> code for visualizations of the dataset
- app.R -> Shiny app for making the results interactive

