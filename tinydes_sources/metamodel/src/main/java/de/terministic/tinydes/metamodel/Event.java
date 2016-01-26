package de.terministic.tinydes.metamodel;

import java.util.ArrayList;
import java.util.List;

/**
 * Models the basic event of an event graph.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class Event {

	private final String name;
	private int priority;
	private List<Transition> transistions;
	private List<StateChange> stateChanges;

	/**
	 * Constructs the event with the priority of 0.
	 * 
	 * @param name event.
	 */
	public Event(String name) {
		this.name = name;
		this.priority = 0;
		this.transistions = new ArrayList<Transition>();
		this.stateChanges = new ArrayList<StateChange>();
	}

	public String getName() {
		return name;
	}

	public int getPriority() {
		return priority;
	}

	public void setPriority(int priority) {
		this.priority = priority;
	}

	public List<Transition> getTransistions() {
		return transistions;
	}

	/**
	 * Adds a {@code Transition} to this event.
	 * 
	 * @param transition
	 */
	public void addTransistion(Transition transition) {
		this.transistions.add(transition);
	}

	public List<StateChange> getStateChanges() {
		return stateChanges;
	}

	/**
	 * Adds a {@code StateChange} to this event.
	 * 
	 * @param stateChange
	 */
	public void addStateChange(StateChange stateChange) {
		this.stateChanges.add(stateChange);
	}
}
