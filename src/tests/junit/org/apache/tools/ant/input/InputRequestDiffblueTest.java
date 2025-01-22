package org.apache.tools.ant.input;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class InputRequestDiffblueTest {
  /**
   * Test {@link InputRequest#InputRequest(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InputRequest#InputRequest(String)}
   */
  @Test
  public void testNewInputRequest_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new InputRequest(null));
  }

  /**
   * Test {@link InputRequest#InputRequest(String)}.
   * <ul>
   *   <li>When {@code Prompt}.</li>
   *   <li>Then return {@code Prompt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InputRequest#InputRequest(String)}
   */
  @Test
  public void testNewInputRequest_whenPrompt_thenReturnPrompt() {
    // Arrange and Act
    InputRequest actualInputRequest = new InputRequest("Prompt");

    // Assert
    assertEquals("Prompt", actualInputRequest.getPrompt());
    assertNull(actualInputRequest.getDefaultValue());
    assertNull(actualInputRequest.getInput());
    assertTrue(actualInputRequest.isInputValid());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link InputRequest#setDefaultValue(String)}
   *   <li>{@link InputRequest#setInput(String)}
   *   <li>{@link InputRequest#getDefaultValue()}
   *   <li>{@link InputRequest#getInput()}
   *   <li>{@link InputRequest#getPrompt()}
   *   <li>{@link InputRequest#isInputValid()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    InputRequest inputRequest = new InputRequest("Prompt");

    // Act
    inputRequest.setDefaultValue("foo");
    inputRequest.setInput("Input");
    String actualDefaultValue = inputRequest.getDefaultValue();
    String actualInput = inputRequest.getInput();
    String actualPrompt = inputRequest.getPrompt();

    // Assert
    assertEquals("Input", actualInput);
    assertEquals("Prompt", actualPrompt);
    assertEquals("foo", actualDefaultValue);
    assertTrue(inputRequest.isInputValid());
  }
}
