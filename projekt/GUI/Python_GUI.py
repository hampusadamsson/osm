import tkinter
import socket
from tkinter import *
import time


pane = tkinter.Tk()
pane.geometry("600x500")
pane.title("Nuntii IRC")

chatHistory = Text(pane)
chatHistory.pack(side = TOP)
temp = StringVar()

def GetTime():
    return "<" + time.strftime("%H:%M")+">: "

def Receive():
    
    chatHistory.insert(INSERT, GetTime() + "Detta funkar inte just nu men är på G!\n")

def Send():
    mtext = temp.get()
    chatHistory.insert(INSERT,GetTime() + mtext+'\n')
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.connect(('localhost', 1337))
    sock.sendall(bytes(mtext,'UTF-8'))
    sock.close()

message = Entry(textvariable = temp)
sendButton = tkinter.Button(pane,text = "Skicka", command = Send)
sendButton.pack(side = BOTTOM)
receiveButton = tkinter.Button(pane,text = "Ta Emot", command = Receive)
receiveButton.pack(side = BOTTOM)
message.pack(side = BOTTOM)

pane.mainloop()
