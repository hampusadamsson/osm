import java.io.IOException;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.Calendar;


public class GUI extends javax.swing.JFrame {

    public GUI() {
        initComponents();
    }

   
    @SuppressWarnings("unchecked")
                           
    private void initComponents() {
         
        jScrollPane1 = new javax.swing.JScrollPane();
        ChatHistory = new javax.swing.JTextArea();
        Message = new javax.swing.JTextField();
        SendButton = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        ChatHistory.setColumns(20);
        ChatHistory.setRows(5);
        jScrollPane1.setViewportView(ChatHistory);

        SendButton.setText("Send");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jScrollPane1)
            .addComponent(Message)
            .addGroup(layout.createSequentialGroup()
                .addGap(222, 222, 222)
                .addComponent(SendButton)
                .addContainerGap(202, Short.MAX_VALUE))
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addComponent(jScrollPane1, javax.swing.GroupLayout.PREFERRED_SIZE, 432, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(Message, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 35, Short.MAX_VALUE)
                .addComponent(SendButton))
        );
       
        //***************************************************************
        //Egna Actionlisteners och initieringar
        //***************************************************************
        try {
            self = new OtpNode("javaNode","test");
            mbox = self.createMbox();
 
            if (self.ping(server, 2000)) {             
            } 
            else {            
                return;
            }
        } catch (IOException e1) {
            e1.printStackTrace();
        }
      
        ChatHistory.setEditable(false);
        
        Message.addActionListener(new ActionListener()
        {
                public void actionPerformed(ActionEvent e){
                       if (!(Message.getText().equals(""))){
                       OtpErlangObject[] msg = new OtpErlangObject[2];
                       msg[0] = mbox.self();
                       msg[1] = new OtpErlangAtom(Message.getText());
                       OtpErlangTuple tuple = new OtpErlangTuple(msg);
                       mbox.send("messageHandler", server, tuple);
                       Message.setText("");
                       }
               }    
        });
               
        SendButton.addActionListener(new ActionListener()
        {
                       public void actionPerformed(ActionEvent e){
                       if (!(Message.getText().equals(""))){
                       OtpErlangObject[] msg = new OtpErlangObject[2];
                       msg[0] = mbox.self();
                       msg[1] = new OtpErlangAtom(Message.getText());
                       OtpErlangTuple tuple = new OtpErlangTuple(msg);
                       mbox.send("messageHandler", server, tuple);
                       Message.setText("");
                       }
               }
        });
        
       ScheduledExecutorService executor = Executors.newScheduledThreadPool(1);    
       executor.scheduleAtFixedRate(PeriodicReceive, 0, 200, TimeUnit.MILLISECONDS);      //Schedular att kolla mailboxen varje sekund m.h.a PeriodicReceive-funktionen
       
       //***************************************************************
        //Slut på egna Actionlisteners och initieringar
        //***************************************************************    
       pack();
    }                       

    /********************************************************************************************************************************************
    * Funktioner för att sköta periodiska uppdateringen av meddelanden från servern
    ********************************************************************************************************************************************/    
    
    public void PeriodicReceiveAux() {
        Thread thread = new Thread()
        {
            public void run(){
                Receive();  
            }
        };
        thread.start();
    }

    Runnable PeriodicReceive = new Runnable() 
    {
        public void run() {
            PeriodicReceiveAux();
        }
    };
    
    public String getTime()
    {
        Calendar t = Calendar.getInstance();
        return "<"+Integer.toString(t.get(Calendar.HOUR_OF_DAY))+":"+Integer.toString(t.get(Calendar.MINUTE))+"> ";
       
    }
     
    public void Receive(){
        try {
            OtpErlangObject rmsg = mbox.receive(1);
            OtpErlangString erlangList = (OtpErlangString) rmsg;
            String temp = erlangList.toString();
            String temp2 = temp.substring(1,temp.length()-1);
            ChatHistory.append(getTime() + temp2 +'\n');			
        }   
        
        catch (NullPointerException e){
                //Lulz @ you NPE
        }
            
        catch (OtpErlangExit e) {
                e.printStackTrace();
                
        } 
        catch (OtpErlangDecodeException e) {
                e.printStackTrace();
        }
    }
  
    public static void main(String args[]) {
        try {
            for (javax.swing.UIManager.LookAndFeelInfo info : javax.swing.UIManager.getInstalledLookAndFeels()) {
                if ("Nimbus".equals(info.getName())) {
                    javax.swing.UIManager.setLookAndFeel(info.getClassName());
                    break;
                }
            }
        } catch (ClassNotFoundException ex) {
            java.util.logging.Logger.getLogger(GUI.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            java.util.logging.Logger.getLogger(GUI.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            java.util.logging.Logger.getLogger(GUI.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        } catch (javax.swing.UnsupportedLookAndFeelException ex) {
            java.util.logging.Logger.getLogger(GUI.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
        }
       
        
        java.awt.EventQueue.invokeLater(new Runnable() 
        {
            public void run() {
                new GUI().setVisible(true);
            }
        });
    }
    
   
   
    
    private OtpNode self;
    private OtpMbox mbox;
    static String server = "server";
    private javax.swing.JTextArea ChatHistory;
    private javax.swing.JTextField Message;
    private javax.swing.JButton SendButton;
    private javax.swing.JScrollPane jScrollPane1;
}
