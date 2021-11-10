let housingDataState = dl.csv('https://raw.githubusercontent.com/matthew-martinez/R/main/rentBurden/rentBurdenStates.csv');

let processedDataSouth = [];
let processedDataWest = [];
let processedDataMidwest = [];
let processedDataNortheast = [];

function getData() {
    for (i = 0; i < housingDataState.length; i++) {
        if(housingDataState[i].Region === "South" && housingDataState[i].State != "District of Columbia"){
            processedDataSouth.push({x: housingDataState[i].rentBurdenPerc, y: housingDataState[i].giniE, z: housingDataState[i].HUEST, name: housingDataState[i].State, region: housingDataState[i].Region});
        }
        if(housingDataState[i].Region === "West"){
            processedDataWest.push({x: housingDataState[i].rentBurdenPerc, y: housingDataState[i].giniE, z: housingDataState[i].HUEST, name: housingDataState[i].State, region: housingDataState[i].Region});
        }
        if(housingDataState[i].Region === "Midwest"){
            processedDataMidwest.push({x: housingDataState[i].rentBurdenPerc, y: housingDataState[i].giniE, z: housingDataState[i].HUEST, name: housingDataState[i].State, region: housingDataState[i].Region});
        }
        if(housingDataState[i].Region === "Northeast"){
            processedDataNortheast.push({x: housingDataState[i].rentBurdenPerc, y: housingDataState[i].giniE, z: housingDataState[i].HUEST, name: housingDataState[i].State, region: housingDataState[i].Region});
        }
    }
}

getData();

Highcharts.chart('container', {

    chart: {
        type: 'bubble'
    },
    title: {
        text: 'Income Inequality & Rent Burden in the United States'
      },
    
      caption: {
        text: 'Data Source: U.S. Census Bureau, American Community Survey, 1-year, 2019. <br> Note: Bubble size represents number of housing units.'
      },
    
      yAxis: {
        title: {
          text: 'Gini Index Score'
        },
        min: 0.42,
        max: 0.54,
        tickInterval: .02
      },
    
      xAxis: {
      title: {
          text: 'Percentage of Households Rent Burdened'
      },
      min: 35,
      max: 60,
      tickInterval: 5
    },

    tooltip: {
        formatter: function() {
            return 'State: ' + this.point.name + '<br>Region: ' + this.series.name +  '<br> Number of Housing Units: ' + this.point.z.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",") + '<br> % of Households Rent Burdened: ' + this.x + '%' + '<br> Gini Index Score: ' + this.y;
        }
    },

    legend: {
        enabled: true,
        align: 'right',
        layout: 'vertical',
        verticalAlign: 'middle',
        itemMarginTop: 10,
        bubbleLegend: {
            enabled: true,
            borderWidth: 1,
            connectorDistance: 40,
            maxSize: 70,
            sizeBy: 'area',
            labels: {
                formatter: function() {
                    return this.value.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
                }
            },
            ranges: [{
                value: 15000000
            }, {
                value: 5000000,
                color: 'rgba(51,160,44, 0.5)',
                borderColor: 'rgb(51,160,44)',
                connectorColor: 'rgb(51,160,44)'
            }, {
                value: 1000000,
                color: 'rgba(255,127,0, 0.5)',
                borderColor: 'rgb(255,127,0)',
                connectorColor: 'rgb(255,127,0)'
            }, {
                value: 500000,
                color: 'rgba(202,178,214 0.5)',
                borderColor: 'rgb(202,178,214)',
                connectorColor: 'rgb(202,178,214)'
            }]
        }
    },

    plotOptions: {
        series: {
            maxSize: 70
        }
    },

    annotations: [{
        labels: [{
            //point: { 
               // x: 28.5, 
                //y: 0.5269,
                //xAxis: 0,
                //yAxis: 0 
            //},
            //text: 'D.C.'
        //},
        //{
            point: {
                x: 38.1, 
                y: 0.46,
                xAxis: 0,
                yAxis: 0 
            },
            text: 'North Dakota'
        },
        {
            point: {
                x: 50.1, 
                y: 0.51,
                xAxis: 0,
                yAxis: 0 
            },
            text: 'New York' 
        },
        {
            point: {
                x: 55.9, 
                y: 0.48,
                xAxis: 0,
                yAxis: 0 
            },
            text: 'Florida' 
        },
        {
            point: {
                x: 44, 
                y: 0.43,
                xAxis: 0,
                yAxis: 0 
            },
            text: 'Utah' 
        },
    ],
        labelOptions: {
            x: -15, y: -12.5
        }
    }],

    series: [{
        name: "Midwest",
        data: processedDataMidwest,
        color: 'rgb(31,120,180)'
    }, {
        name: "Northeast",
        data: processedDataNortheast,
        color: 'rgb(51,160,44)'
    }, {
        name: "South",
        data: processedDataSouth,
        color: 'rgb(255,127,0)'
    }, {
        name: "West",
        data: processedDataWest,
        color: 'rgb(202,178,214)'
    }]

});
