package existence;

import coupling.Experiment;
import coupling.Result;
import coupling.interaction.Interaction;
import coupling.interaction.Interaction020;
import java.lang.Math.*;

/**
 * An Existence020 is a sort of Existence010 in which each Interaction has a predefined Valence.
 * When a given Experience is performed and a given Result is obtained, the corresponding Interaction is considered enacted.
 * The Existence020 is PLEASED when the enacted Interaction has a positive or null Valence, and PAINED otherwise.
 * An Existence020 is still a single entity rather than being split into an explicit Agent and Environment. 
 * An Existence020 demonstrates a rudimentary decisional mechanism and a rudimentary learning mechanism.
 * It learns to choose the Experience that induces an Interaction that has a positive valence.  
 * Try to change the Valences of interactions and the method giveResult(experience) 
 * and observe that the Existence020 still learns to enact interactions that have positive valences.  
 */
public class Existence020 extends Existence010 {
	
	//private Interaction previousInteraction;
	private Interaction previousInteraction;
	
	/**Didn't quite work right and changes ended up producing errors in later extensions...*/
	@Override
	protected void initExistence(){
		Experiment e1 = addOrGetExperience(LABEL_E1);
		Experiment e2 = addOrGetExperience(LABEL_E2);
		//Experiment e3 = addOrGetExperience(LABEL_E3);
		//Experiment e4 = addOrGetExperience(LABEL_E4);
		Result r1 = createOrGetResult(LABEL_R1);
		Result r2 = createOrGetResult(LABEL_R2);
		//Result r3 = createOrGetResult(LABEL_R3);
		//Result r4 = createOrGetResult(LABEL_R4);
		/** Change the valence of interactions to change the agent's motivation */
		addOrGetPrimitiveInteraction(e1, r1, -1);  
		addOrGetPrimitiveInteraction(e1, r2, 1);
		addOrGetPrimitiveInteraction(e2, r1, -1);
		addOrGetPrimitiveInteraction(e2, r2, 1);
		//addOrGetPrimitiveInteraction(e3, r3, -1);
		//addOrGetPrimitiveInteraction(e3, r4, 1);	
		//addOrGetPrimitiveInteraction(e4, r1, -1);
		//addOrGetPrimitiveInteraction(e4, r2, 1);
		//addOrGetPrimitiveInteraction(e4, r4, -1);
		//addOrGetPrimitiveInteraction(e4, r3, 1);	
		this.setPreviousExperience(e1);
		this.setPreviousInteraction((Interaction020)this.addOrGetPrimitiveInteraction(e1,r1));
	}
	
	@Override
	public String step() {
		
		Experiment experience = this.getPreviousExperience();
		if (this.getMood() == Mood.PAINED || this.getMood() == Mood.BORED){
			experience = getOtherExperience(experience);		
			this.setSelfSatisfactionCounter(0);
		}
		Result result = returnResult010(experience);
	
		Interaction020 enactedInteraction = (Interaction020)this.addOrGetPrimitiveInteraction(experience, result);
		
		if (enactedInteraction == this.getPreviousInteraction())
			this.incSelfSatisfactionCounter();
		this.setPreviousInteraction(enactedInteraction);
		
		if (this.getSelfSatisfactionCounter() >= BOREDOME_LEVEL)
			this.setMood(Mood.BORED);
		else if (enactedInteraction.getValence() /**- (Math.random()*2.2-1.1)*/ >= 0)
			this.setMood(Mood.PLEASED);
		else
			this.setMood(Mood.PAINED);

		this.setPreviousExperience(experience);
		
		return experience.getLabel() + result.getLabel() + " " + this.getMood();
	}
	
	/**
	 * Create an interaction as a tuple <experience, result>.
	 * @param experience: The experience.
	 * @param result: The result.
	 * @param valence: the interaction's valence
	 * @return The created interaction
	 */
	protected Interaction020 addOrGetPrimitiveInteraction(Experiment experience, Result result, int valence) {
		String label = experience.getLabel() + result.getLabel();
		if (!INTERACTIONS.containsKey(label)){
			Interaction020 interaction = createInteraction(label);
			interaction.setExperience(experience);
			interaction.setResult(result);
			interaction.setValence(valence);
			INTERACTIONS.put(label, interaction);			
		}
		Interaction020 interaction = (Interaction020)INTERACTIONS.get(label);
		return interaction;
	}
		
	@Override
	protected Interaction020 createInteraction(String label){
		return new Interaction020(label);
	}

	@Override
	protected Interaction020 getInteraction(String label){
		return (Interaction020)INTERACTIONS.get(label);
	}
	
	public Interaction getPreviousInteraction() {
		return previousInteraction;
	}
	public void setPreviousInteraction(Interaction previousInteraction) {
		this.previousInteraction = previousInteraction;
	}
	
}
