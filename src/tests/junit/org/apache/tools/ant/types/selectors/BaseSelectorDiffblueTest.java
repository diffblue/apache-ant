package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.junit.Test;

public class BaseSelectorDiffblueTest {
  /**
   * Test {@link BaseSelector#setError(String, Throwable)} with {@code msg}, {@code cause}.
   * <ul>
   *   <li>Given {@link ScriptSelector} (default constructor).</li>
   *   <li>Then {@link ScriptSelector} (default constructor) Error is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelector#setError(String, Throwable)}
   */
  @Test
  public void testSetErrorWithMsgCause_givenScriptSelector_thenScriptSelectorErrorIsMsg() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();

    // Act
    scriptSelector.setError("Msg", new Throwable());

    // Assert
    assertEquals("Msg", scriptSelector.getError());
  }

  /**
   * Test {@link BaseSelector#setError(String, Throwable)} with {@code msg}, {@code cause}.
   * <ul>
   *   <li>Then {@link ScriptSelector} (default constructor) Error is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelector#setError(String, Throwable)}
   */
  @Test
  public void testSetErrorWithMsgCause_thenScriptSelectorErrorIsFoo() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();
    scriptSelector.setError("foo");

    // Act
    scriptSelector.setError("Msg", new Throwable());

    // Assert that nothing has changed
    assertEquals("foo", scriptSelector.getError());
  }

  /**
   * Test {@link BaseSelector#setError(String)} with {@code msg}.
   * <ul>
   *   <li>Given {@link ScriptSelector} (default constructor) Error is {@code foo}.</li>
   *   <li>Then {@link ScriptSelector} (default constructor) Error is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelector#setError(String)}
   */
  @Test
  public void testSetErrorWithMsg_givenScriptSelectorErrorIsFoo_thenScriptSelectorErrorIsFoo() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();
    scriptSelector.setError("foo");

    // Act
    scriptSelector.setError("Msg");

    // Assert that nothing has changed
    assertEquals("foo", scriptSelector.getError());
  }

  /**
   * Test {@link BaseSelector#setError(String)} with {@code msg}.
   * <ul>
   *   <li>Given {@link ScriptSelector} (default constructor).</li>
   *   <li>Then {@link ScriptSelector} (default constructor) Error is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelector#setError(String)}
   */
  @Test
  public void testSetErrorWithMsg_givenScriptSelector_thenScriptSelectorErrorIsMsg() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();

    // Act
    scriptSelector.setError("Msg");

    // Assert
    assertEquals("Msg", scriptSelector.getError());
  }

  /**
   * Test {@link BaseSelector#getError()}.
   * <p>
   * Method under test: {@link BaseSelector#getError()}
   */
  @Test
  public void testGetError() {
    // Arrange, Act and Assert
    assertNull((new ScriptSelector()).getError());
  }

  /**
   * Test {@link BaseSelector#validate()}.
   * <ul>
   *   <li>Given {@link ScriptSelector} (default constructor) Error is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelector#validate()}
   */
  @Test
  public void testValidate_givenScriptSelectorErrorIsFoo_thenThrowBuildException() {
    // Arrange
    ScriptSelector scriptSelector = new ScriptSelector();
    scriptSelector.setError("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptSelector.validate());
  }
}
