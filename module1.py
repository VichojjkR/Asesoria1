from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd
import numpy as np
import requests
import re #re es para expresiones regulares y glob para leer dire
import nltk #librería natural language toolkit
from nltk.tokenize import RegexpTokenizer #para tokenizar con expresiones regulares
from nltk.corpus import stopwords #para quitar stopwords
import gensim #modelos de semantica latente
from gensim.models.ldamodel import LdaModel
from gensim.models.coherencemodel import CoherenceModel
import pyLDAvis #para visualizar los modelos de topicos
import pyLDAvis.gensim_models as gensimvis
import re
from sentiment_analysis_spanish import sentiment_analysis
import string
import spacy
from spacy.lang.es.examples import sentences 
nlp = spacy.load('es_core_news_sm')
from PIL import Image
import matplotlib.pyplot as plt
from wordcloud import WordCloud
from pysentimiento import create_analyzer
analyzer = create_analyzer(task="sentiment", lang="es")

def tele13(x,t='wea'):
    url=x
    page=urlopen(url)
    html = page.read().decode("utf-8")
    soup=BeautifulSoup(html,'html.parser')
    cajita=soup.find('div', id='article-body-wrapper',class_="article-component__body")
    Titulo=soup.find(class_="article-component__header-title")
    parrafo=cajita.find('p').get_text()
    allp=cajita.find_all('p')
    if t=='Titulo':
        Noticia=[Titulo.get_text()]
        for a in range(len(allp)):
                Noticia.append(allp[a].get_text())
        vec=[Noticia[0]]
    else:
        Noticia=['a']
        for a in range(len(allp)):
                Noticia.append(allp[a].get_text())
        vec=[Noticia]
    return vec

#Función para obtener los textos y crea un vector que contiene la info
def chv(x,t='wea'):
    url=x
    page=urlopen(url)
    html = page.read().decode("utf-8")
    soup=BeautifulSoup(html,'html.parser')
    cajita=soup.find('div',class_='the-single-section__text the-single-section-text')
    Titulo=soup.title
    parrafo=cajita.find('p').get_text()
    allp=cajita.find_all('p')
    if t=='Titulo':
        Noticia=[Titulo.get_text()]
        for a in range(len(allp)):
                Noticia.append(allp[a].get_text())
        vec=[Noticia[0]]
    else:
        Noticia=['a']
        for a in range(len(allp)):
                Noticia.append(allp[a].get_text())
        vec=[Noticia]
    return vec
#
def tvn(x,t='wea'):
    url=x
    page=requests.get(url).text
    soup=BeautifulSoup(page,'html.parser')
    cajita=soup.find('div',class_="CUERPO")
    Titulo=soup.find('div', class_='titular')
    parrafo=cajita.find('p').get_text()
    allp=cajita.find_all('p')
    if t=='Titulo':
        Noticia=[Titulo.get_text()]
        for a in range(len(allp)):
                Noticia.append(allp[a].get_text())
        vec=[Noticia[0]]
    else:
        Noticia=['a']
        for a in range(len(allp)):
                Noticia.append(allp[a].get_text())
        vec=[Noticia]
    return vec

#Función para obtener los textos y crea un vector que contiene la info
def mega(x,t='wea'):
    url=x
    page=urlopen(url)
    html = page.read().decode("utf-8")
    soup=BeautifulSoup(html,'html.parser')
    cajita=soup.find('div',class_="contenido-nota")
    Titulo=soup.find('h1')
    parrafo=cajita.find('p').get_text()
    allp=cajita.find_all('p')
    if t=='Titulo':
        Noticia=[Titulo.get_text()]
        for a in range(len(allp)):
                Noticia.append(allp[a].get_text())
        vec=[Noticia[0]]
    else:
        Noticia=['a']
        for a in range(len(allp)):
                Noticia.append(allp[a].get_text())
        vec=[Noticia]
    return vec


def dataT13(x,y,t='wea'):
    n=len(x)
    econo=np.empty((n, 0)).tolist()
    for i in range(len(x)):
        econo[i]=tele13(x[i],t)
    df1=pd.DataFrame(econo,columns=[y])
    return df1

def dataTVN(x,y,t='wea'):
    n=len(x)
    econo=np.empty((n, 0)).tolist()
    for i in range(len(x)):
        econo[i]=tvn(x[i],t)
    df1=pd.DataFrame(econo,columns=[y])
    return df1

def dataMega(x,y,t='wea'):
    n=len(x)
    econo=np.empty((n, 0)).tolist()
    for i in range(len(x)):
        econo[i]=mega(x[i],t)
    df1=pd.DataFrame(econo,columns=[y])
    return df1

def dataCHV(x,y,t='wea'):
    n=len(x)
    econo=np.empty((n, 0)).tolist()
    for i in range(len(x)):
        econo[i]=chv(x[i],t)
    df1=pd.DataFrame(econo,columns=[y])
    return df1



sentimiento = sentiment_analysis.SentimentAnalysisSpanish()
def ScoreSentimiento(x):
    n1=x.shape[1]
    n2=x.shape[0]
    for i in range(0,n1):
        print(" ","\n")
        print(x.columns[i],"\n")
        print(" ","\n")
        for j in range(0,n2):
            print(x.iloc[j,i])
            print(sentimiento.sentiment(x.iloc[j,i]))
    return


def AllNoti0(x,t='wea',unit=False):
    if t=='Titular':
        dT13=dataT13(x['T13'],'T13','Titulo')
        dTtvn=dataTVN(x['TVN'],'TVN','Titulo')
        dTmega=dataMega(x['MEGA'],'Mega','Titulo')
        dTchv=dataCHV(x['CHV'],'CHV','Titulo')
        data0=dT13
        data0['TVN']=dTtvn
        data0['MEGA']=dTmega
        data0["CHV"]=dTchv
        data0=data0.astype(str)
        return data0
        
    else:
        if unit==False:
            d13=dataT13(x['T13'],'T13')
            dtvn=dataTVN(x['TVN'],'TVN')
            dmega=dataMega(x['MEGA'],'Mega')
            dchv=dataCHV(x['CHV'],'CHV')
            data0=d13
            data0['TVN']=dtvn
            data0['MEGA']=dmega
            data0["CHV"]=dchv
            data0=data0.astype(str)
            data0=pd.DataFrame(data0.apply(lambda x: ' '.join(map(str, x)), axis=0)).transpose()
            data0=[data0['T13'],data0['TVN'],data0['MEGA'],data0['CHV']]
            data0=pd.DataFrame(data0)
            data0.columns=["Noticias"]
        else:
            d13=dataT13(x['T13'],'T13')
            dtvn=dataTVN(x['TVN'],'TVN')
            dmega=dataMega(x['MEGA'],'Mega')
            dchv=dataCHV(x['CHV'],'CHV')
            data0=pd.DataFrame(d13,columns=['T13'])
            data0['TVN']=dtvn
            data0['MEGA']=dmega
            data0["CHV"]=dchv
            data0=data0.astype(str)
        return data0
    
def normalize(s):
    replacements = (
        ("á", "a"),
        ("é", "e"),
        ("í", "i"),
        ("ó", "o"),
        ("ú", "u"),
    )
    for a, b in replacements:
        s = s.replace(a, b).replace(a.upper(), b.upper())
    return s



def limpieza1(texto):
    for i in range(0,len(texto)):
        texto[i]=normalize(texto[i])
        texto[i]=texto[i].lower()
    return texto

def limpieza2(texto):
    texto= re.sub(r'\n\n24horas.cl\nATON\n\n\n\n\n',' ',texto)
    texto= re.sub(r'\n24horas.cl\n',' ',texto)
    texto= re.sub(r'\n',' ', texto)
    texto= re.sub(r'\\n',' ', texto)
    texto= re.sub(r'chvnoticias.cl',' ',texto)
    texto=re.sub('[%s]' % re.escape(string.punctuation), ' ',texto)
    texto=re.sub('\w*\d\w*', ' ',texto)
    texto=re.sub(r'\W+', ' ', texto)
    return texto
Lp2 = lambda x:limpieza2(x)


def AllNoti1(x,t='wea',unit=False):
    if unit==False:
        y=AllNoti0(x,t)
    else:
        y=AllNoti0(x,t,unit=True)
    if t=='Titular':
        y=pd.DataFrame([y.T13.apply(Lp2),
                              y.TVN.apply(Lp2),
                              y.MEGA.apply(Lp2),
                              y.CHV.apply(Lp2)]).transpose() 
        return y
    else:
        y=pd.DataFrame(y.Noticias.apply(Lp2))
        y.columns=['Noticias']
        y=pd.DataFrame(limpieza1(y.Noticias))
        y.columns=['Noticias']
        return y
    
dicc_palabras = {
   'carabineros':'carabinero','muerte':'muerto','muere':'muerto','morir':'muerto','delincuent':'delincuente',
   'disparo':'disparar','dispar':'disparar','encerronar':'encerrona','fallecer':'muerto','fallece':'muerto',
   'policia':'policial'
} 
def Remplace(x):
    for i in range(len(x)):
        for p in range(len(dicc_palabras)):
            x[i] = x[i].replace(list(dicc_palabras.keys())[p] ,list(dicc_palabras.values())[p])
    return x

def AllNoti(x,t='wea',token=False,unit=False):
    tokenizer = RegexpTokenizer(r'\w+')
    if t=='Titular':
        if unit==False:
            y=AllNoti1(x,t)
        else:
            y=AllNoti1(x,t,unit=True)
        if token==True:
            y=pd.DataFrame(y.apply(lambda x: ' '.join(map(str, x)), axis=0))
            y.columns=['Titular']
            y=pd.DataFrame(Remplace(y.Titular),index=['T13','TVN','MEGA','CHV'],columns=['Titular'])
            y=[tokenizer.tokenize(Texto) for Texto in y.Titular] 
            return y 
        else:
           return y
       
    else:
        y=AllNoti1(x,t)
        if token==True:
            y=pd.DataFrame(Remplace(y.Noticias),index=['T13','TVN','MEGA','CHV'],columns=['Noticias'])
            y=[tokenizer.tokenize(Texto) for Texto in y.Noticias] 
            return y 
        else:
           return y
       
def CleanStopW(x,stop1):
    for i in range(len(x)):
        x[i]=[word for word in x[i] if word not in stop1]
    return x



def leman(x,y):
    n=len(x)
    x=CleanStopW(x,y)
    for i in range(0,n): 
        text = ' '.join(x[i])
        doc = nlp(text)
        lemmas = [tok.lemma_.lower() for tok in doc]
        x[i]= [tok.lemma_.lower() for tok in doc if tok.pos_ != 'PRON']
    return x

def Scorepysentimiento(x):
    n1=x.shape[1]
    n2=x.shape[0]
    for i in range(0,n1):
        print(" ","\n")
        print(x.columns[i],"\n")
        print(" ","\n")
        for j in range(0,n2):
            print(x.iloc[j,i])
            print(analyzer.predict(x.iloc[j,i]))

    return


def TopWords(x,n=20):
    print('Top T13','\n')
    frecuencia = nltk.FreqDist(x[0])
    frecuencia.plot(n,cumulative=False)
    print('Top TVN','\n')
    frecuencia = nltk.FreqDist(x[1])
    frecuencia.plot(n,cumulative=False)
    print('Top MEGA','\n')
    frecuencia = nltk.FreqDist(x[2])
    frecuencia.plot(n,cumulative=False)
    print('Top CHV','\n')
    frecuencia = nltk.FreqDist(x[3])
    frecuencia.plot(n,cumulative=False)
    
