package edu.gatech.eilab.scheherazade.server;

import java.io.IOException;
import java.util.ArrayList;

import org.eclipse.jetty.websocket.api.Session;
import org.eclipse.jetty.websocket.api.annotations.OnWebSocketClose;
import org.eclipse.jetty.websocket.api.annotations.OnWebSocketConnect;
import org.eclipse.jetty.websocket.api.annotations.OnWebSocketError;
import org.eclipse.jetty.websocket.api.annotations.OnWebSocketMessage;
import org.eclipse.jetty.websocket.api.annotations.WebSocket;

import edu.gatech.eilab.scheherazade.generation.*;


@WebSocket
public class MyWebSocketHandler {
	InteractiveEngine ie;
	Session mySession;
	
	private final String PART_ONE = "Storyyyyy";
	private final String PART_TWO = "Tableeee";
	private final String PART_THREE = "Listeeeee";
	private final String PART_FOUR = "Surveyyyy";
	
	private ArrayList<String> readout;
	
    @OnWebSocketClose
    public void onClose(int statusCode, String reason) {
        System.out.println("Close: statusCode=" + statusCode + ", reason=" + reason);
    }

    @OnWebSocketError
    public void onError(Throwable t) {
        System.out.println("Error: " + t.getMessage());
    }

    @OnWebSocketConnect
    public void onConnect(Session session) {
        //System.out.println("Connect: " + session.getRemoteAddress().getAddress());
        ie = new InteractiveEngine();
    	ie.play("movie");
    	
    	mySession = session;
    }

    @OnWebSocketMessage
    public void onMessage(String message) {
    	if(message.contains(PART_ONE)){
    		message = message.substring(PART_ONE.length());
    		HandleStoryMessage(message);
    	}
    	else if(message.contains(PART_TWO)){
    		message = message.substring(PART_TWO.length());
    		//System.out.println("All errors: "+message);
    	}
    	else if(message.contains(PART_THREE)){
    		message = message.substring(PART_THREE.length());
    		
    		if(message.contains("get")){
	    		String toSend = "";
	    		
	    		for(int i = 0; i<readout.size(); i++){
	    			toSend+=readout.get(i);
	    		}
    		
	    		try {
	                 mySession.getRemote().sendString(PART_THREE+toSend);
	             } catch (IOException e) {
	                 e.printStackTrace();
	             }
    		}
    	}
    }
    
    public void HandleStoryMessage(String message){
    	
    	int toInt = Integer.parseInt(message);
    	if(toInt>=-1 && toInt<=10){
        	String toExec = ie.execute(toInt);
        	
        	if(toExec!=null && toExec.length()!=0){
        	 try {
        		 readout.add(toExec);
                 mySession.getRemote().sendString(PART_ONE+toExec);
             } catch (IOException e) {
                 e.printStackTrace();
             }
        	}
        }
        else if(toInt==-2){
        	if(readout!=null && readout.size()!=0){
        		 ie = new InteractiveEngine();
        		 ie.play("movie");
        	}
        	
        	readout = new ArrayList<String>();
        	String desc = ie.GetDescription();
        	 
        	if(desc!=null && desc.length()!=0){
        		 try {
        			 readout.add(desc);
                     mySession.getRemote().sendString(PART_ONE+desc);
                 } catch (IOException e) {
                     e.printStackTrace();
                 }
        	}
        }
    }
}
