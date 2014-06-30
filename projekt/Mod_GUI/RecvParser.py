from tkinter import *
import TabUtils
import time
import ConnectionUtils
import MiscUtils

def checkQueue(gui):
        """

        Parses the answer from the server and performes the corresponding 
        command.

        Side effects: 

        Can be one of the following: A new tab is created, 
        the username is changed, a message is printed in the current window.
        """
        
        stopSign = 1
        respons = gui.thread.returnQueue()
        
        if(respons[0][0] == "{"):
            options = initDictionary()
            temp = respons[1:len(respons)-2]
            if " " in temp:
                commandString = MiscUtils.messageSplit(temp)
                if commandString[0] == 'disconnected':
                    stopSign = 0
                    disconnected(gui)
                else:
                    if commandString[0] in options:
                        options[commandString[0]](gui,commandString[1])
                    else:
                       fillUserList(gui,commandString)   
            else: 
                fillRoomList(gui,temp)                   
        else:
            printMessage(gui,respons)
            
        if stopSign == 1:
            gui.master.after(50,checkQueue,gui)

######################################################################################################

def success(gui,arguments):
        command = MiscUtils.messageSplit(arguments)
        gui.rooms[command[0]] = [command[0],command[1]]
        TabUtils.addTab(gui,command[0])

######################################################################################################

def invited(gui,arguments):
        if(MiscUtils.noDuplicate(gui.windowList,arguments)):
                TabUtils.addTab(gui,arguments)

######################################################################################################

def whois(gui,arguments):
        
        whoisInfo = arguments.split(",")
        TabUtils.writeMessage(gui,"")
        TabUtils.writeMessage(gui,"------------------------")
        for element in whoisInfo:
                TabUtils.writeMessage(gui,element)
        TabUtils.writeMessage(gui,"------------------------")
        TabUtils.writeMessage(gui,"")

######################################################################################################

def track(gui,arguments):
    
        trackList = arguments.split(",")
        TabUtils.writeMessage(gui,"")
        TabUtils.writeMessage(gui,"-------------------------------------")
        TabUtils.writeMessage(gui,"The user is a member of:")
        for element in trackList:
                TabUtils.writeMessage(gui,element)
        TabUtils.writeMessage(gui,"-------------------------------------")
        TabUtils.writeMessage(gui,"")

######################################################################################################

def username(gui,arguments):
        gui.configList["userName"] = arguments
        gui.menues.setName(arguments)
        MiscUtils.rename(gui)

######################################################################################################

def disconnected(gui):
        gui.socketStatus = "disconnected"
        ConnectionUtils.disconnect(gui)

        if gui.configList["reconnectMode"] == 'auto':
                TabUtils.writeMessage(gui,"Lost the connection, trying to reconnect automaticly")
                ConnectionUtils.connect(gui)
        else:
                TabUtils.writeMessage(gui,"Lost the connection. Use /connect IP to connect manually")

######################################################################################################

def fillRoomList(gui,argument):
        roomList = argument.split(",")
        gui.roomWindow.delete(0,END)
        for room in roomList:    
                gui.roomWindow.insert(END,room)

######################################################################################################

def fillUserList(gui,arguments):
        gui.userList[arguments[0]] = arguments[1].split(",")
        if (arguments[0] == gui.currentTab):
                TabUtils.fillUserList(gui)

######################################################################################################

def printMessage(gui,message):
        argumentString = MiscUtils.messageSplit(message)          
        gui.windowList[argumentString[0]].config(state=NORMAL)
        gui.windowList[argumentString[0]].insert(END,MiscUtils.GetTime() + argumentString[1])
        gui.windowList[argumentString[0]].yview(END)
        gui.windowList[argumentString[0]].config(state=DISABLED)

######################################################################################################

def empty(*args):
        pass

######################################################################################################

def error(gui,arguments):
        TabUtils.writeMessage(gui,"The room is private, you need an invitation")

######################################################################################################

def initDictionary():
        dictionary = {
                'success': success,
                'empty': empty,
                'error': error,
                'invited': invited,
                'whois': whois,
                'track': track,
                'username': username,
        }
        return dictionary
