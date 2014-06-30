from tkinter import *

####################################################################################### 

def deleteAllTabs(gui):
        """

        deletes all tabs except for global.
        """
        for window in gui.windowList:
            if window != "global":
                deleteTab(gui,window)

####################################################################################### 

def deleteTab(gui,name):
        """

        Deletes the selected tab.

        Keyword arguments:
        name - The name of the tab to be deleted.
        """
        gui.nb.forget(gui.windowList[name])
        gui.menues.setRoomList(gui.windowList)

####################################################################################### 

def addTab(gui,name):

        """

        Creates a new tab and corrensponding chat window and stores them

        Keyword arguments:
        name - The name of the tab
        """    
        tab = Text(gui.master,state=DISABLED)
        tab.config(background = "#121A16",foreground="#00EB00")
        gui.windowList[name] = tab
        gui.menues.updateRoomList(gui.windowList)
        gui.nb.add(tab, text=name)
        gui.menues.setRoomList(gui.windowList)

####################################################################################### 

def clearWindowList(windowList):
        """

        Clears the entire windowList except for global. 
        """
        copyList = []
        for element in windowList:
            if element != "global":
                copyList.append(copy.deepcopy(element))    
        for key in copyList:
            if key == "global":
                continue
            else:
                windowList.pop(key,None)

####################################################################################### 

def restoreTabs(gui):
        """

        Restores all the rooms the user was in.
        """
        for element in gui.windowList:
            if element != "global":
                temp = element + " " + "/invite "+gui.configList["userName"] + '\n'
                msg = temp.encode('UTF-8')
                gui.serverSocket.send(msg)       
                
####################################################################################### 

def clearWindow(window):
        """

        Removes all text from the currently active window.
        """
        window.config(state=NORMAL)
        window.delete("1.0",END)
        window.config(state=DISABLED)

####################################################################################### 

def tabChangedEvent(event,gui):
        """

        Changes to the current tab.

        Keyword arguments:
        event - User clicks on a new tab.
        """
        gui.currentTab = event.widget.tab(event.widget.index("current"),"text")
        fillUserList(gui)

####################################################################################### 

def fillUserList(gui):
        """

        Fill the userList with the usernames in the current room.

        Keyword arguments:
        roomName - In which to list the users.
        """
        gui.userWindow.delete(0,END)
        for userName in gui.userList[gui.currentTab]:     
            gui.userWindow.insert(END,userName)

####################################################################################### 
    
def writeMessage(gui,message):
        """

        Displays the message in the currently active window.
        If the flag is set to syscall the text color is set to red. 
        """
        gui.windowList[gui.currentTab].config(state=NORMAL)
        gui.windowList[gui.currentTab].insert(END,message+'\n')
        gui.windowList[gui.currentTab].tag_add("here", "end -2 line linestart", "end -2 line lineend")
        gui.windowList[gui.currentTab].tag_config("here", background="#121A16", foreground="red")
        gui.windowList[gui.currentTab].yview(END)
        gui.windowList[gui.currentTab].config(state=DISABLED)
