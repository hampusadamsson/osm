import tkinter
import socket
from tkinter import *
import time
import sys
from threading import Thread
import select
import threading
from multiprocessing import Process
import queue
from time import sleep

###################################################
#Importerar nuvarande tiden och returnerar den
###################################################

queue = queue.Queue()

def GetTime():
    return "<" + time.strftime("%H:%M")+">: "

##########################################################
#Kollar om det finns något nytt att hämta på socketen
#Om det inte gör det kommer körningen outtimas efter 0.1
#sekunder, annars genomförs hämtningen och läggs till i
#chathistoriken
##########################################################

def getRespons():

    data = sockSend.recv(512)
    respons = str(data,encoding='UTF-8')
    queue.put(respons)
    getRespons()

def checkQueue():
    if (queue.empty()):
        pane.after(50,checkQueue)
    else: 
        respons = queue.get()
        chatHistory.config(state=NORMAL)
        chatHistory.insert(INSERT,GetTime() + respons)
        chatHistory.config(state=DISABLED)
        pane.after(50,checkQueue)

##################################################################
#Hämtar texten från entryfältet och skickar den i bytearray-format
#till socketen. Tömmer sedan entryfältet
##################################################################

def sendMessage(event):
     
    mtext1 = temp.get()
    if (mtext1 != ""):
        mtext = mtext1+'\n'
        msg = mtext.encode('UTF-8')
        sockSend.send(msg)
        message.delete(0,END)

##################################################################
#Samma som ovan, men utan event-argumentet då detta ej behövs
#vid command = sendMessage på knapppen
##################################################################

def sendMessageButton():
     
    mtext1 = temp.get()
    if (mtext1 != ""):
        mtext = mtext1+'\n'
        msg = mtext.encode('UTF-8')
        sockSend.send(msg)
        message.delete(0,END)

###################################################
#Initierar huvudframen för GUI:t och sätter titeln
###################################################
        
pane = tkinter.Tk()
pane.geometry("600x500")
pane.title("Nuntii IRC")

###################################################
#Lägger till ett textfält och scrollbar
###################################################

scrollBar = Scrollbar(pane)
scrollBar.pack(side = RIGHT, fill = Y)
chatHistory = Text(pane, yscrollcommand=scrollBar.set,state=DISABLED)
chatHistory.pack(side = TOP, fill=BOTH)
scrollBar.config(command=chatHistory.yview)

#########################################################################
#Stringvariabel so används för att hämta texten från inmatningsfältet
#########################################################################

temp = StringVar()

#########################################################################
#Öppnar socketen till servern på adressen, på port 1337
#########################################################################

sockSend = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
#sockSend.connect(('46.246.18.140', 1337))
#sockSend.connect(('130.243.209.116', 1337))
sockSend.connect(('localhost', 1337))


###################################################
#Lägger till en send-knapp
###################################################

sendButton = tkinter.Button(pane,text = "Skicka", command = sendMessageButton)
sendButton.pack(side = BOTTOM)

##################################################################
#Lägger till ett inmatningsfält, och binder Enterknappen till det
##################################################################

message = Entry(textvariable = temp,width=40)
message.pack(side = BOTTOM)
message.bind('<Return>',sendMessage)

##########################################################
#Startar bakgrundsfunktionen som kontinuerligt kollar om
#vi har fått nytt meddelande
##########################################################

sleep(0.05)
thread = Thread(target=getRespons)
thread.start()
checkQueue()
print("Funkar nog inte")
#recButton = tkinter.Button(pane,text = "Rec", command = checkQueue)
#recButton.pack(side = BOTTOM)

###################################################
#Startar GUI:t
###################################################

pane.mainloop()


