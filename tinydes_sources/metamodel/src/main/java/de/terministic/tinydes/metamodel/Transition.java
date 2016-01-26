package de.terministic.tinydes.metamodel;

/**
 * Model a transition to a target event.
 * 
 * @author de.terministic group
 * @version 1.0
 */
public class Transition {

	private final Event targetEvent;
	private Condition condition;
	private Delay delay;
	private boolean inhibiting;

	/**
	 * Creates a transition to given target event. This transitions has the
	 * following default properties:
	 * <ul>
	 * 	<li>
	 * 		The condition is always true. 
	 * 		{@code Condition} is {@code TrueCondition}
	 * 	</li>
	 * 	<li>
	 * 		There is no delay. 
	 * 		{@code Delay} is {@code ZeroDelay}
	 * 	</li>
	 * </ul>
	 * 
	 * @param targetEvent
	 */
	public Transition(Event targetEvent) {
		this.targetEvent = targetEvent;
		this.condition = new TrueCondition();
		this.delay = new ZeroDelay();
		this.inhibiting = false;
	}

	public Event getTargetEvent() {
		return targetEvent;
	}

	public Condition getCondition() {
		return condition;
	}

	public void setCondition(Condition condition) {
		this.condition = condition;
	}

	public Delay getDelay() {
		return delay;
	}

	public void setDelay(Delay delay) {
		this.delay = delay;
	}

	public boolean isInhibiting() {
		return inhibiting;
	}

	public void setInhibiting(boolean inhibiting) {
		this.inhibiting = inhibiting;
	}

}