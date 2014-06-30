import MiscUtils
from tkinter import *
import copy
import ConnectionUtils
import TabUtils

def sendParser(gui,message):

        """

        Retrieves the text from the entry field, parses it and sends it to 
        the server in a bytearray form. 

        Keyword arguments: 
        event - The return-key was pressent

        Side-effects:
        Checks if the command is valid. If so, sends it to the server
        """
        options = initiateSwitch()
        argumentString = MiscUtils.messageSplit(message)

        if (argumentString[0] == "/connect"):
                connect(argumentString,gui)
                    
        elif (gui.socketStatus != "ok"):
            TabUtils.writeMessage(gui,"You are not connected to a server, use /connect IP")
            gui.message.delete(0,END)
                    
        else:
            if argumentString[0] in options:
                options[argumentString[0]](argumentString,gui)
            else:
                msg_temp = gui.currentTab + " " + message+'\n'
                sendToServer(gui,msg_temp)

#####################################################################################################

def connect(argument,gui):
        if(gui.socketStatus == "disconnected"):
               if gui.configList["ipAdress"] != argument[1]:
                    TabUtils.deleteAllTabs(gui)
                    TabUtils.clearWindowList(gui.windowList)
               gui.configList["ipAdress"] = argument[1]
               ConnectionUtils.connect(gui)
        else:
                    
                if argument[1] == gui.configList["ipAdress"]:
                    TabUtils.writeMessage(gui,"You are already connected to " +argument[1])
                else:
                    ConnectionUtils.disconnect(gui)
                    TabUtils.deleteAllTabs(gui)
                    TabUtils.clearWindowList(gui.windowList)
                    gui.configList["ipAdress"] = argument[1]
                    if gui.configList["reconnectMode"] != "auto":
                        gui.master.after(100,ConnectionUtils.connect,gui)
                gui.message.delete(0,END)

#####################################################################################################

def join(argument,gui):
            success = 0
            if (" " in argument[1]):
                argumentString2 = MiscUtils.messageSplit(argument[1])
                if MiscUtils.noDuplicate(gui.windowList,argumentString2[0]):
                   success = 1
                arguments = 2
            else:
                if MiscUtils.noDuplicate(gui.windowList,argument[1]):
                    success = 1
                arguments = 1
            if success == 1:
                if (arguments == 2):
                    msg_temp = argument[0] + " " + argument[0]+ " " + argument[1]+'\n'
                       
                else:
                    msg_temp = argument[1] + " " + argument[0]+ " " + argument[1]+'\n'    
                sendToServer(gui,msg_temp)
            else:
                TabUtils.writeMessage(gui,"You are already a member of that room!")
                gui.message.delete(0,END)

#####################################################################################################

def invite(argument,gui):
        msg_temp = gui.currentTab + " " + argument[0] + " " +argument[1] +'\n'
        sendToServer(gui,msg_temp)

#####################################################################################################

def clear(argument,gui):
        TabUtils.clearWindow(gui.windowList[gui.currentTab])
        gui.message.delete(0,END)

#####################################################################################################

def rename(argument,gui):
        msg_temp = gui.currentTab + " " + argument[0] + " " +argument[1] +'\n'
        sendToServer(gui,msg_temp)

#####################################################################################################

def leave(argument,gui):
        if argument[1] == "global":
            TabUtils.writeMessage("You cannot leave global!")
            gui.message.delete(0,END)
        else:
            if(not MiscUtils.noDuplicate(gui.windowList,argument[1])):
                TabUtils.deleteTab(gui,argument[1])
                gui.windowList.pop(argument[1],None)
                gui.menues.updateRoomList(gui.windowList)
                msg_temp ="global" + " " + argument[0] + " " +argument[1] +'\n'
                sendToServer(gui,msg_temp)
            else:
                gui.writeMessage("You are not a member of the room " + argument[1] +"!")
                gui.message.delete(0,END)

#####################################################################################################

def initiateSwitch():
        dictionary = {
            "/join": join,
            "/invite": invite,
            "/clea":  clear,
            "/rename": rename,
            "/exit": leave,
        }
        return dictionary

#####################################################################################################

def sendToServer(gui,message):
        msg = message.encode('UTF-8')
        gui.serverSocket.send(msg)
        gui.message.delete(0,END)
