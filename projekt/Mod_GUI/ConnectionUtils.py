import sys
import socket
from tkinter import *
import ConnectParser
import ConnectThread

####################################################################################### 

def closeClient(gui):
        """

        Shuts down the connection and window when the user closes the window
        """
        if gui.socketStatus == "ok":            
            gui.serverSocket.shutdown(socket.SHUT_RDWR)
        gui.serverSocket.close()
        gui.master.destroy()
        sys.exit(0)

####################################################################################### 

def connect(gui):
        """

        Starts the connect thread and initiate the checkConnectQueue function.
        """
        gui.message.config(state=DISABLED)
        thread = ConnectThread.connectToServer(gui.serverSocket,gui.configList["ipAdress"],gui.configList["delay"],gui.configList["reconnectAmount"])
        thread.daemon = True
        thread.start()
        ConnectParser.checkConnectQueue(gui,thread)

####################################################################################### 

def disconnect(gui):
        """

        Terminates the connectin to the server.
        """
        if gui.socketStatus == "ok":            
                gui.serverSocket.shutdown(socket.SHUT_RDWR)
        gui.serverSocket.close()
        gui.serverSocket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        gui.socketStatus = "disconnected"
     
####################################################################################### 

def sendUserName(gui):
        """

        Sends the userName to the server. 
        """
        userName = gui.configList["userName"]
        temp = userName+'\n'
        msg = temp.encode('UTF-8')
        gui.serverSocket.send(msg)

#######################################################################################
           
def timeout_update(gui):
        if gui.socketStatus == "ok":
            text = "/global " + "timeout_update\n"
            msg = text.encode('UTF-8')
            gui.serverSocket.send(msg)
            gui.master.after(30000,timeout_update,gui)
        else:
            pass
        
