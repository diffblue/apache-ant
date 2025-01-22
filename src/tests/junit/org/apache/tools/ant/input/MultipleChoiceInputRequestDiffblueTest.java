package org.apache.tools.ant.input;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Vector;
import org.junit.Test;

public class MultipleChoiceInputRequestDiffblueTest {
  /**
   * Test {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Collection)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code Prompt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Collection)}
   */
  @Test
  public void testNewMultipleChoiceInputRequest_given42_whenArrayListAdd42_thenReturnPrompt() {
    // Arrange
    ArrayList<String> choices = new ArrayList<>();
    choices.add("42");
    choices.add("foo");

    // Act
    MultipleChoiceInputRequest actualMultipleChoiceInputRequest = new MultipleChoiceInputRequest("Prompt", choices);

    // Assert
    assertEquals("Prompt", actualMultipleChoiceInputRequest.getPrompt());
    assertNull(actualMultipleChoiceInputRequest.getDefaultValue());
    assertNull(actualMultipleChoiceInputRequest.getInput());
    assertEquals(choices, actualMultipleChoiceInputRequest.getChoices());
  }

  /**
   * Test {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Vector)}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Vector#Vector()} add {@code 42}.</li>
   *   <li>Then return {@code Prompt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Vector)}
   */
  @Test
  public void testNewMultipleChoiceInputRequest_given42_whenVectorAdd42_thenReturnPrompt() {
    // Arrange
    Vector<String> choices = new Vector<>();
    choices.add("42");
    choices.add("foo");

    // Act
    MultipleChoiceInputRequest actualMultipleChoiceInputRequest = new MultipleChoiceInputRequest("Prompt", choices);

    // Assert
    assertEquals("Prompt", actualMultipleChoiceInputRequest.getPrompt());
    assertNull(actualMultipleChoiceInputRequest.getDefaultValue());
    assertNull(actualMultipleChoiceInputRequest.getInput());
    assertEquals(choices, actualMultipleChoiceInputRequest.getChoices());
  }

  /**
   * Test {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Collection)}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code foo}.</li>
   *   <li>Then return {@code Prompt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Collection)}
   */
  @Test
  public void testNewMultipleChoiceInputRequest_givenFoo_whenArrayListAddFoo_thenReturnPrompt() {
    // Arrange
    ArrayList<String> choices = new ArrayList<>();
    choices.add("foo");

    // Act
    MultipleChoiceInputRequest actualMultipleChoiceInputRequest = new MultipleChoiceInputRequest("Prompt", choices);

    // Assert
    assertEquals("Prompt", actualMultipleChoiceInputRequest.getPrompt());
    assertNull(actualMultipleChoiceInputRequest.getDefaultValue());
    assertNull(actualMultipleChoiceInputRequest.getInput());
    assertEquals(choices, actualMultipleChoiceInputRequest.getChoices());
  }

  /**
   * Test {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Vector)}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>When {@link Vector#Vector()} add {@code foo}.</li>
   *   <li>Then return {@code Prompt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Vector)}
   */
  @Test
  public void testNewMultipleChoiceInputRequest_givenFoo_whenVectorAddFoo_thenReturnPrompt() {
    // Arrange
    Vector<String> choices = new Vector<>();
    choices.add("foo");

    // Act
    MultipleChoiceInputRequest actualMultipleChoiceInputRequest = new MultipleChoiceInputRequest("Prompt", choices);

    // Assert
    assertEquals("Prompt", actualMultipleChoiceInputRequest.getPrompt());
    assertNull(actualMultipleChoiceInputRequest.getDefaultValue());
    assertNull(actualMultipleChoiceInputRequest.getInput());
    assertEquals(choices, actualMultipleChoiceInputRequest.getChoices());
  }

  /**
   * Test {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Collection)}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return {@code Prompt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Collection)}
   */
  @Test
  public void testNewMultipleChoiceInputRequest_whenArrayList_thenReturnPrompt() {
    // Arrange
    ArrayList<String> choices = new ArrayList<>();

    // Act
    MultipleChoiceInputRequest actualMultipleChoiceInputRequest = new MultipleChoiceInputRequest("Prompt", choices);

    // Assert
    assertEquals("Prompt", actualMultipleChoiceInputRequest.getPrompt());
    assertNull(actualMultipleChoiceInputRequest.getDefaultValue());
    assertNull(actualMultipleChoiceInputRequest.getInput());
    assertEquals(choices, actualMultipleChoiceInputRequest.getChoices());
  }

  /**
   * Test {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Collection)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Collection)}
   */
  @Test
  public void testNewMultipleChoiceInputRequest_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class,
        () -> new MultipleChoiceInputRequest("Prompt", (Collection<String>) null));

  }

  /**
   * Test {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Vector)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Vector)}
   */
  @Test
  public void testNewMultipleChoiceInputRequest_whenNull_thenThrowIllegalArgumentException2() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> new MultipleChoiceInputRequest("Prompt", (Vector<String>) null));

  }

  /**
   * Test {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Vector)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then return {@code Prompt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#MultipleChoiceInputRequest(String, Vector)}
   */
  @Test
  public void testNewMultipleChoiceInputRequest_whenVector_thenReturnPrompt() {
    // Arrange
    Vector<String> choices = new Vector<>();

    // Act
    MultipleChoiceInputRequest actualMultipleChoiceInputRequest = new MultipleChoiceInputRequest("Prompt", choices);

    // Assert
    assertEquals("Prompt", actualMultipleChoiceInputRequest.getPrompt());
    assertNull(actualMultipleChoiceInputRequest.getDefaultValue());
    assertNull(actualMultipleChoiceInputRequest.getInput());
    assertEquals(choices, actualMultipleChoiceInputRequest.getChoices());
  }

  /**
   * Test {@link MultipleChoiceInputRequest#getChoices()}.
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#getChoices()}
   */
  @Test
  public void testGetChoices() {
    // Arrange, Act and Assert
    assertTrue((new MultipleChoiceInputRequest("Prompt", new ArrayList<>())).getChoices().isEmpty());
  }

  /**
   * Test {@link MultipleChoiceInputRequest#isInputValid()}.
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#isInputValid()}
   */
  @Test
  public void testIsInputValid() {
    // Arrange
    MultipleChoiceInputRequest multipleChoiceInputRequest = new MultipleChoiceInputRequest("Prompt", new ArrayList<>());
    multipleChoiceInputRequest.setInput("Input");

    // Act and Assert
    assertFalse(multipleChoiceInputRequest.isInputValid());
  }

  /**
   * Test {@link MultipleChoiceInputRequest#isInputValid()}.
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#isInputValid()}
   */
  @Test
  public void testIsInputValid2() {
    // Arrange
    MultipleChoiceInputRequest multipleChoiceInputRequest = new MultipleChoiceInputRequest("Prompt", new ArrayList<>());
    multipleChoiceInputRequest.setDefaultValue("foo");
    multipleChoiceInputRequest.setInput("");

    // Act and Assert
    assertTrue(multipleChoiceInputRequest.isInputValid());
  }

  /**
   * Test {@link MultipleChoiceInputRequest#isInputValid()}.
   * <ul>
   *   <li>Given {@link ArrayList#ArrayList()} add empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#isInputValid()}
   */
  @Test
  public void testIsInputValid_givenArrayListAddEmptyString_thenReturnTrue() {
    // Arrange
    ArrayList<String> choices = new ArrayList<>();
    choices.add("");

    MultipleChoiceInputRequest multipleChoiceInputRequest = new MultipleChoiceInputRequest("Prompt", choices);
    multipleChoiceInputRequest.setInput("");

    // Act and Assert
    assertTrue(multipleChoiceInputRequest.isInputValid());
  }

  /**
   * Test {@link MultipleChoiceInputRequest#isInputValid()}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MultipleChoiceInputRequest#isInputValid()}
   */
  @Test
  public void testIsInputValid_thenReturnFalse() {
    // Arrange
    MultipleChoiceInputRequest multipleChoiceInputRequest = new MultipleChoiceInputRequest("Prompt", new ArrayList<>());
    multipleChoiceInputRequest.setInput("");

    // Act and Assert
    assertFalse(multipleChoiceInputRequest.isInputValid());
  }
}
