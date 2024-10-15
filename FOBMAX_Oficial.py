#%%
import pandas as pd
import openpyxl
import matplotlib.pyplot as plt
import plotly.express as px

bolsa = pd.read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Input/base_precios_bolsa.xlsx")
bolsa = bolsa.dropna(subset=["Precio"]).rename(columns={"Precio": "FOB_Bolsa"})
bolsa["Fecha"] = pd.to_datetime(bolsa["Fecha"], format="%d/%m/%Y")
bolsa["Producto"].replace("Trigo 11.5%", "Trigo", inplace=True)


minagro = pd.read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Input/serie_precios_minagro.xlsx")
minagro = minagro.dropna(subset=["Precio"]).query("Precio != 0").rename(columns={"Precio": "FOB_Minagro"})
minagro["Fecha"] = pd.to_datetime(minagro["Fecha"], format="%d/%m/%Y")
minagro["Cereal"].replace("Trigo Pan", "Trigo", inplace=True)


bolsa['FOB_max_Bolsa'] = bolsa[['precio_comprador', 'precio_vendedor']].max(axis=1)

# Agrupa por Año, Mes y Producto para encontrar el valor máximo
resultado_maximo = bolsa.groupby(['Fecha', 'Producto','Año', 'Mes'])['FOB_max_Bolsa'].max().reset_index()

# cambiar nombre de la columna Producto por Cereal
resultado_maximo = resultado_maximo.rename(columns={"Producto": "Cereal"})

# Combinar los DataFrames 'bolsa' y 'minagro'
todo = pd.merge(resultado_maximo, minagro)

todo["posicion"] = pd.to_datetime(todo["Año"].astype(str) + '-' + todo["Mes"].astype(str) + '-01')

#Crear una columna con las diferencia de meses 
todo['Fecha'] = pd.to_datetime(todo['Fecha'])
todo['posicion'] = pd.to_datetime(todo['posicion'])

todo['time_delta_months'] = todo['posicion'].dt.to_period('M').view(dtype='int64') - todo['Fecha'].dt.to_period('M').view(dtype='int64')

todo['Embarque'] =  't+' + todo['time_delta_months'].astype(str)

todo['Diferencia_Minagro_BdC'] = todo['FOB_Minagro'] - todo['FOB_max_Bolsa']

todo.to_excel("C:/Users/evasquez/Downloads/diferencias_ministerio_bolsaMAX.xlsx", index=False)



resultado = todo[todo['Embarque'].isin(['t+0', 't+1','t+2','t+3','t+4'])]
#no incluir el último día del mes de la columna fecha

resultado = resultado[resultado['Fecha'] != resultado['Fecha'] + pd.offsets.MonthEnd(0)]

resultado = resultado.melt(id_vars=['Fecha', 'Cereal', 'Embarque'], value_vars=['FOB_Minagro', 'FOB_max_Bolsa'], var_name='Fuente', value_name='Precio')

#Quiero las fechas despues del 29 de septiembre del 2023
resultado = resultado[resultado['Fecha'] > '2023-09-29']

# para cada producto crear graficos interactivo de resultado2 los precios de FOB_Minagro, FOB_Bolsa y Valor por embarques t+0, t+1, t+2, t+3 y t+4 y guardarlos en un archivo html
for producto in resultado['Cereal'].unique():
    resultado_producto = resultado[resultado['Cereal'] == producto]
    fig2 = px.line(resultado_producto, x='Fecha', y='Precio', color='Fuente', facet_col='Embarque',
                  title='Precios ' + producto,
                  labels={'Precio': 'Precio', 'Fecha': 'Fecha', 'Fuente': 'Fuente'})
    fig2.show()
    fig2.write_html("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Graficos/precios_max" + producto + ".html")


