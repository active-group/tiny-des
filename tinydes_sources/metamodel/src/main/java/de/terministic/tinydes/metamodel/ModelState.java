package de.terministic.tinydes.metamodel;

import java.util.HashMap;
import java.util.Map;

/**
 * Represents the model states as a collection of state variables.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class ModelState {
	
	private Map<String, Object> states;
	
	/**
	 * Constructor.
	 */
	public ModelState() {
		this.states = new HashMap<String, Object>();
	}
	
	public Map<String, Object> getStates() {
		return states;
	}
}
