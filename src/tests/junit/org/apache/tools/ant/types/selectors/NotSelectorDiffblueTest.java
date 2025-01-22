package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.junit.Test;

public class NotSelectorDiffblueTest {
  /**
   * Test {@link NotSelector#NotSelector()}.
   * <p>
   * Method under test: {@link NotSelector#NotSelector()}
   */
  @Test
  public void testNewNotSelector() {
    // Arrange and Act
    NotSelector actualNotSelector = new NotSelector();

    // Assert
    Location location = actualNotSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualNotSelector.getDescription());
    assertNull(actualNotSelector.getError());
    assertNull(actualNotSelector.getProject());
    assertNull(actualNotSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualNotSelector.isReference());
  }

  /**
   * Test {@link NotSelector#NotSelector(FileSelector)}.
   * <p>
   * Method under test: {@link NotSelector#NotSelector(FileSelector)}
   */
  @Test
  public void testNewNotSelector2() {
    // Arrange and Act
    NotSelector actualNotSelector = new NotSelector(new ScriptSelector());

    // Assert
    Location location = actualNotSelector.getLocation();
    assertNull(location.getFileName());
    assertNull(actualNotSelector.getDescription());
    assertNull(actualNotSelector.getError());
    assertNull(actualNotSelector.getProject());
    assertNull(actualNotSelector.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualNotSelector.isReference());
  }

  /**
   * Test {@link NotSelector#toString()}.
   * <ul>
   *   <li>Given {@link NotSelector#NotSelector()} addNone {@link NoneSelector} (default constructor).</li>
   *   <li>Then return {@code {notselect: {noneselect: }}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NotSelector#toString()}
   */
  @Test
  public void testToString_givenNotSelectorAddNoneNoneSelector_thenReturnNotselectNoneselect() {
    // Arrange
    NotSelector notSelector = new NotSelector();
    notSelector.addNone(new NoneSelector());

    // Act and Assert
    assertEquals("{notselect: {noneselect: }}", notSelector.toString());
  }

  /**
   * Test {@link NotSelector#toString()}.
   * <ul>
   *   <li>Given {@link NotSelector#NotSelector()} addNone {@link NoneSelector} (default constructor).</li>
   *   <li>Then return {@code {notselect: {noneselect: , }}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NotSelector#toString()}
   */
  @Test
  public void testToString_givenNotSelectorAddNoneNoneSelector_thenReturnNotselectNoneselect2() {
    // Arrange
    NotSelector notSelector = new NotSelector();
    notSelector.addSelector(new SelectSelector());
    notSelector.addNone(new NoneSelector());

    // Act and Assert
    assertEquals("{notselect: {noneselect: , }}", notSelector.toString());
  }

  /**
   * Test {@link NotSelector#toString()}.
   * <ul>
   *   <li>Given {@link NotSelector#NotSelector()} addNot {@link NotSelector#NotSelector()}.</li>
   *   <li>Then return {@code {notselect: {noneselect: }}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NotSelector#toString()}
   */
  @Test
  public void testToString_givenNotSelectorAddNotNotSelector_thenReturnNotselectNoneselect() {
    // Arrange
    NotSelector notSelector = new NotSelector();
    notSelector.addNot(new NotSelector());

    // Act and Assert
    assertEquals("{notselect: {noneselect: }}", notSelector.toString());
  }

  /**
   * Test {@link NotSelector#toString()}.
   * <ul>
   *   <li>Given {@link NotSelector#NotSelector()} addNot {@link NotSelector#NotSelector()}.</li>
   *   <li>Then return {@code {notselect: {noneselect: , }}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NotSelector#toString()}
   */
  @Test
  public void testToString_givenNotSelectorAddNotNotSelector_thenReturnNotselectNoneselect2() {
    // Arrange
    NotSelector notSelector = new NotSelector();
    notSelector.addSelector(new SelectSelector());
    notSelector.addNot(new NotSelector());

    // Act and Assert
    assertEquals("{notselect: {noneselect: , }}", notSelector.toString());
  }

  /**
   * Test {@link NotSelector#toString()}.
   * <ul>
   *   <li>Given {@link NotSelector#NotSelector()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link NotSelector#toString()}
   */
  @Test
  public void testToString_givenNotSelector_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new NotSelector()).toString());
  }

  /**
   * Test {@link NotSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {notselect: {noneselect: }}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NotSelector#toString()}
   */
  @Test
  public void testToString_thenReturnNotselectNoneselect() {
    // Arrange
    NotSelector notSelector = new NotSelector();
    notSelector.addSelector(new SelectSelector());

    // Act and Assert
    assertEquals("{notselect: {noneselect: }}", notSelector.toString());
  }

  /**
   * Test {@link NotSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {notselect: {noneselect: , }}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NotSelector#toString()}
   */
  @Test
  public void testToString_thenReturnNotselectNoneselect2() {
    // Arrange
    NotSelector notSelector = new NotSelector();
    notSelector.addSelector(new SelectSelector());
    notSelector.addSelector(new SelectSelector());

    // Act and Assert
    assertEquals("{notselect: {noneselect: , }}", notSelector.toString());
  }

  /**
   * Test {@link NotSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {notselect: {noneselect: ScriptSelector}}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NotSelector#toString()}
   */
  @Test
  public void testToString_thenReturnNotselectNoneselectScriptSelector() {
    // Arrange, Act and Assert
    assertEquals("{notselect: {noneselect: ScriptSelector}}", (new NotSelector(new ScriptSelector())).toString());
  }

  /**
   * Test {@link NotSelector#toString()}.
   * <ul>
   *   <li>Then return {@code {notselect: {noneselect: {select ScriptSelector}}}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NotSelector#toString()}
   */
  @Test
  public void testToString_thenReturnNotselectNoneselectSelectScriptSelector() {
    // Arrange
    SelectSelector selector = new SelectSelector();
    selector.appendSelector(new ScriptSelector());

    NotSelector notSelector = new NotSelector();
    notSelector.addSelector(selector);

    // Act and Assert
    assertEquals("{notselect: {noneselect: {select ScriptSelector}}}", notSelector.toString());
  }

  /**
   * Test {@link NotSelector#verifySettings()}.
   * <p>
   * Method under test: {@link NotSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings() {
    // Arrange
    NotSelector notSelector = new NotSelector();

    // Act
    notSelector.verifySettings();

    // Assert
    assertEquals("One and only one selector is allowed within the <not> tag", notSelector.getError());
  }

  /**
   * Test {@link NotSelector#verifySettings()}.
   * <ul>
   *   <li>Then {@link NotSelector#NotSelector(FileSelector)} with other is {@link ScriptSelector} (default constructor) Error is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NotSelector#verifySettings()}
   */
  @Test
  public void testVerifySettings_thenNotSelectorWithOtherIsScriptSelectorErrorIsNull() {
    // Arrange
    NotSelector notSelector = new NotSelector(new ScriptSelector());

    // Act
    notSelector.verifySettings();

    // Assert that nothing has changed
    assertNull(notSelector.getError());
  }
}
