package org.apache.tools.ant.input;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import org.junit.Test;

public class DefaultInputHandlerDiffblueTest {
  /**
   * Test {@link DefaultInputHandler#getPrompt(InputRequest)}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>Then return {@code Prompt [foo]}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultInputHandler#getPrompt(InputRequest)}
   */
  @Test
  public void testGetPrompt_givenFoo_thenReturnPromptFoo() {
    // Arrange
    DefaultInputHandler defaultInputHandler = new DefaultInputHandler();

    InputRequest request = new InputRequest("Prompt");
    request.setDefaultValue("foo");

    // Act and Assert
    assertEquals("Prompt [foo]", defaultInputHandler.getPrompt(request));
  }

  /**
   * Test {@link DefaultInputHandler#getPrompt(InputRequest)}.
   * <ul>
   *   <li>Given {@code (}.</li>
   *   <li>Then return {@code Prompt ( ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultInputHandler#getPrompt(InputRequest)}
   */
  @Test
  public void testGetPrompt_givenLeftParenthesis_thenReturnPrompt() {
    // Arrange
    DefaultInputHandler defaultInputHandler = new DefaultInputHandler();

    ArrayList<String> choices = new ArrayList<>();
    choices.add(" (");

    // Act and Assert
    assertEquals("Prompt ( ()", defaultInputHandler.getPrompt(new MultipleChoiceInputRequest("Prompt", choices)));
  }

  /**
   * Test {@link DefaultInputHandler#getPrompt(InputRequest)}.
   * <ul>
   *   <li>Given {@code )}.</li>
   *   <li>Then return {@code Prompt (), ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultInputHandler#getPrompt(InputRequest)}
   */
  @Test
  public void testGetPrompt_givenRightParenthesis_thenReturnPrompt() {
    // Arrange
    DefaultInputHandler defaultInputHandler = new DefaultInputHandler();

    ArrayList<String> choices = new ArrayList<>();
    choices.add(")");
    choices.add(" (");

    // Act and Assert
    assertEquals("Prompt (),  ()", defaultInputHandler.getPrompt(new MultipleChoiceInputRequest("Prompt", choices)));
  }

  /**
   * Test {@link DefaultInputHandler#getPrompt(InputRequest)}.
   * <ul>
   *   <li>Then return {@code Prompt ()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultInputHandler#getPrompt(InputRequest)}
   */
  @Test
  public void testGetPrompt_thenReturnPrompt() {
    // Arrange
    DefaultInputHandler defaultInputHandler = new DefaultInputHandler();

    // Act and Assert
    assertEquals("Prompt ()",
        defaultInputHandler.getPrompt(new MultipleChoiceInputRequest("Prompt", new ArrayList<>())));
  }

  /**
   * Test {@link DefaultInputHandler#getPrompt(InputRequest)}.
   * <ul>
   *   <li>Then return {@code Prompt ([ (])}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultInputHandler#getPrompt(InputRequest)}
   */
  @Test
  public void testGetPrompt_thenReturnPrompt2() {
    // Arrange
    DefaultInputHandler defaultInputHandler = new DefaultInputHandler();

    ArrayList<String> choices = new ArrayList<>();
    choices.add(" (");

    MultipleChoiceInputRequest request = new MultipleChoiceInputRequest("Prompt", choices);
    request.setDefaultValue(" (");

    // Act and Assert
    assertEquals("Prompt ([ (])", defaultInputHandler.getPrompt(request));
  }

  /**
   * Test {@link DefaultInputHandler#getPrompt(InputRequest)}.
   * <ul>
   *   <li>When {@link InputRequest#InputRequest(String)} with {@code Prompt}.</li>
   *   <li>Then return {@code Prompt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultInputHandler#getPrompt(InputRequest)}
   */
  @Test
  public void testGetPrompt_whenInputRequestWithPrompt_thenReturnPrompt() {
    // Arrange
    DefaultInputHandler defaultInputHandler = new DefaultInputHandler();

    // Act and Assert
    assertEquals("Prompt", defaultInputHandler.getPrompt(new InputRequest("Prompt")));
  }
}
