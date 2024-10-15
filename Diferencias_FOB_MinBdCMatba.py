#%%
import pandas as pd
import openpyxl
import matplotlib.pyplot as plt
import plotly.express as px


# Leer el archivo Excel "serie_fob.xlsx"
bolsa = pd.read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Input/serie_fob.xlsx")
bolsa = bolsa.dropna(subset=["Precio"]).rename(columns={"Precio": "FOB_Bolsa"})
bolsa["Fecha"] = pd.to_datetime(bolsa["Fecha"], format="%d/%m/%Y")
bolsa["Cereal"].replace("Trigo 11.5%", "Trigo", inplace=True)


# Leer el archivo Excel "serie_precios_minagro.xlsx"
minagro = pd.read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Input/serie_precios_minagro.xlsx")
minagro = minagro.dropna(subset=["Precio"]).query("Precio != 0").rename(columns={"Precio": "FOB_Minagro"})
minagro["Fecha"] = pd.to_datetime(minagro["Fecha"], format="%d/%m/%Y")
minagro["Cereal"].replace("Trigo Pan", "Trigo", inplace=True)

# Combinar los DataFrames 'bolsa' y 'minagro'
todo = pd.merge(bolsa, minagro)

# Crear una nueva columna 'posicion'
todo["posicion"] = pd.to_datetime(todo["Año"].astype(str) + '-' + todo["Mes"].astype(str) + '-01')

#Crear una columna con las diferencia de meses 
todo['Fecha'] = pd.to_datetime(todo['Fecha'])
todo['posicion'] = pd.to_datetime(todo['posicion'])

todo['time_delta_months'] = todo['posicion'].dt.to_period('M').view(dtype='int64') - todo['Fecha'].dt.to_period('M').view(dtype='int64')

todo['Embarque'] =  't+' + todo['time_delta_months'].astype(str)


matba_soja= pd.read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Input/posiciones_MATBA.xlsx", sheet_name="MATBA Soja")
matba_maiz= pd.read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Input/posiciones_MATBA.xlsx", sheet_name="MATBA MAIZ")
matba_trigo= pd.read_excel("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Input/posiciones_MATBA.xlsx", sheet_name="MATBA trigo")

matba = pd.concat([matba_soja, matba_maiz], ignore_index=True)
matba = pd.concat([matba, matba_trigo], ignore_index=True)
matba['Fecha'] = pd.to_datetime(matba['Fecha'])

resultado = pd.merge(todo, matba, how='left')

resultado['Diferencia_MinagroBdC'] = resultado['FOB_Minagro'] - resultado['FOB_Bolsa']


resultado.to_excel("C:/Users/evasquez/Downloads/diferencias_ministerio_bolsa_matba.xlsx", index=False)


#Graficos - DIFERENCIAS por posicion FOB MINAGRO - BOLSA 
resultado['Fecha'] = pd.to_datetime(resultado['Fecha'])

# Define las condiciones de filtrado
condiciones = (resultado['Embarque'].isin(['t+0', 't+1','t+2','t+3','t+4'])) & (resultado['Fecha'] > '2023-09-01')

# Aplica las condiciones al DataFrame
resultado1 = resultado[condiciones]



# Supongamos que tienes un DataFrame llamado 'graficos_vendedor' con las mismas columnas y datos en Python

embarques = resultado1['Embarque'].unique()

for embarque in embarques:
    # Filtra los datos para el tipo de embarque actual
    subset_data = resultado1[resultado1['Embarque'] == embarque]
    
    # Crea un gráfico interactivo de líneas
    fig = px.line(subset_data, x='Fecha', y='Diferencia_MinagroBdC', color='Cereal',
                 title=f'SAGyP-Bolsa - {embarque}')
    
    fig.update_layout(
        xaxis_title='Fecha',
        yaxis_title='Diferencias'
    )
    
    fig.show()
    fig.write_html("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Graficos/diferencias_" + embarque + ".html")


#Graficos - Precios 
# Convertir a pivot longer a las columnas FOB_Minagro, FOB_Bolsa y Valor del dataframe 'resultado'
resultado2 = resultado.melt(id_vars=['Fecha', 'Cereal', 'Embarque'], value_vars=['FOB_Minagro', 'FOB_Bolsa', 'Matba'], var_name='Fuente', value_name='Precio')

#graficar resultado2 para fechas mayores a 2023-09-01
resultado2 = resultado2[resultado2['Fecha'] > '2024-05-01']

# para cada producto crear graficos interactivo de resultado2 los precios de FOB_Minagro, FOB_Bolsa y Valor por embarques t+0, t+1, t+2, t+3 y t+4 y guardarlos en un archivo html
for producto in resultado2['Cereal'].unique():
    resultado2_producto = resultado2[resultado2['Cereal'] == producto]
    fig2 = px.line(resultado2_producto, x='Fecha', y='Precio', color='Fuente', facet_col='Embarque',
                  title='Precios ' + producto,
                  labels={'Precio': 'Precio', 'Fecha': 'Fecha', 'Fuente': 'Fuente'})
    fig2.show()
    fig2.write_html("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Graficos/precios_" + producto + ".html")

### 3 posible grafico
# filtrar resultado2 por embarque t+0, t+1, t+2, t+3 
resultado3 = resultado2[resultado2['Embarque'].isin(['t+0', 't+1','t+2','t+3','t+4','t+5','t+6','t+7','t+8'])]
#no incluir el último día del mes de la columna fecha

resultado3= resultado3[resultado3['Fecha'] != resultado3['Fecha'] + pd.offsets.MonthEnd(0)]

# para cada producto crear graficos interactivo de resultado2 los precios de FOB_Minagro, FOB_Bolsa y Valor por embarques t+0, t+1, t+2, t+3 y t+4 y guardarlos en un archivo html
for producto in resultado3['Cereal'].unique():
    resultado3_producto = resultado3[resultado3['Cereal'] == producto]
    fig2 = px.line(resultado3_producto, x='Fecha', y='Precio', color='Fuente', facet_col='Embarque',
                  title='Precios ' + producto,
                  labels={'Precio': 'Precio', 'Fecha': 'Fecha', 'Fuente': 'Fuente'})
    fig2.show()
    fig2.write_html("C:/Users/evasquez/OneDrive - Bolsa de Cereales de Buenos Aires/Documentos - Instituto Estudios Económicos/Pedidos/2024/Diferencias_FOB/Graficos/precios2_" + producto + ".html")


# quiero una tabla que me muestre las diferencias de precios entre FOB_Minagro y FOB_Bolsa para cada producto y embarque




# Filtramos los tipos de embarque que nos interesan
resultado4 = resultado[resultado['Embarque'].isin(['t+0', 't+1', 't+2', 't+3', 't+4', 't+5', 't+6', 't+7',  't+8' ])]

# Nos aseguramos de excluir los registros donde la fecha coincide con el final del mes
resultado4 = resultado4[resultado['Fecha'] != resultado['Fecha'] + pd.offsets.MonthEnd(0)]

# Extraemos el mes y el año de la fecha para el agrupamiento
resultado4['Mes'] = resultado4['Fecha'].dt.to_period('M')

# Agrupamos por Cereal, Embarque y Mes, y calculamos la media de las diferencias
tabla_diferencias = resultado4.groupby(['Cereal', 'Embarque', 'Mes'])['Diferencia_MinagroBdC'].mean().reset_index()

# Ordenamos los resultados por Mes para una mejor visualización
tabla_diferencias.sort_values(by='Mes', inplace=True)

#muestrame la tabla en formato dataframe
print(tabla_diferencias)

# Guardamos la tabla en un archivo Exce
tabla_diferencias.to_excel("C:/Users/evasquez/Downloads/tabla_diferencias.xlsx", index=False)