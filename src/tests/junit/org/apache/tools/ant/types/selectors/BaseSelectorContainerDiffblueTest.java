package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.junit.Test;

public class BaseSelectorContainerDiffblueTest {
  /**
   * Test {@link BaseSelectorContainer#hasSelectors()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor) appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#hasSelectors()}
   */
  @Test
  public void testHasSelectors_givenAndSelectorAppendSelectorScriptSelector_thenReturnTrue() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertTrue(andSelector.hasSelectors());
  }

  /**
   * Test {@link BaseSelectorContainer#hasSelectors()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#hasSelectors()}
   */
  @Test
  public void testHasSelectors_givenAndSelector_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new AndSelector()).hasSelectors());
  }

  /**
   * Test {@link BaseSelectorContainer#selectorCount()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor) appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#selectorCount()}
   */
  @Test
  public void testSelectorCount_givenAndSelectorAppendSelectorScriptSelector_thenReturnOne() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals(1, andSelector.selectorCount());
  }

  /**
   * Test {@link BaseSelectorContainer#selectorCount()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#selectorCount()}
   */
  @Test
  public void testSelectorCount_givenAndSelector_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new AndSelector()).selectorCount());
  }

  /**
   * Test {@link BaseSelectorContainer#getSelectors(Project)}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#getSelectors(Project)}
   */
  @Test
  public void testGetSelectors_givenAndSelector_thenReturnArrayLengthIsZero() {
    // Arrange
    AndSelector andSelector = new AndSelector();

    // Act and Assert
    assertEquals(0, andSelector.getSelectors(new Project()).length);
  }

  /**
   * Test {@link BaseSelectorContainer#getSelectors(Project)}.
   * <ul>
   *   <li>Then return array length is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#getSelectors(Project)}
   */
  @Test
  public void testGetSelectors_thenReturnArrayLengthIsOne() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    ScriptSelector selector = new ScriptSelector();
    andSelector.appendSelector(selector);

    // Act
    FileSelector[] actualSelectors = andSelector.getSelectors(new Project());

    // Assert
    assertEquals(1, actualSelectors.length);
    assertSame(selector, actualSelectors[0]);
  }

  /**
   * Test {@link BaseSelectorContainer#toString()}.
   * <p>
   * Method under test: {@link BaseSelectorContainer#toString()}
   */
  @Test
  public void testToString() {
    // Arrange
    ScriptSelector selector = new ScriptSelector();
    selector.setDescription("The characteristics of someone or something");

    AndSelector andSelector = new AndSelector();
    andSelector.appendSelector(selector);

    // Act and Assert
    assertEquals("{andselect: ScriptSelector The characteristics of someone or something}", andSelector.toString());
  }

  /**
   * Test {@link BaseSelectorContainer#toString()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor) addAnd {@link AndSelector} (default constructor).</li>
   *   <li>Then return {@code {andselect: , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#toString()}
   */
  @Test
  public void testToString_givenAndSelectorAddAndAndSelector_thenReturnAndselectScriptSelector() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.addAnd(new AndSelector());
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: , ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link BaseSelectorContainer#toString()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#toString()}
   */
  @Test
  public void testToString_givenAndSelector_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new AndSelector()).toString());
  }

  /**
   * Test {@link BaseSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code {andselect: {dateselector date: null compare: equal granularity: 1000}}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnAndselectDateselectorDateNullCompareEqualGranularity1000() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.addDate(new DateSelector());

    // Act and Assert
    assertEquals("{andselect: {dateselector date: null compare: equal granularity: 1000}}", andSelector.toString());
  }

  /**
   * Test {@link BaseSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code {andselect: ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnAndselectScriptSelector() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link BaseSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code {andselect: , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnAndselectScriptSelector2() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.addSelector(new SelectSelector());
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: , ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link BaseSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code {andselect: ScriptSelector, ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnAndselectScriptSelectorScriptSelector() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.appendSelector(new ScriptSelector());
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: ScriptSelector, ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link BaseSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code {andselect: ScriptSelector, , ScriptSelector}}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnAndselectScriptSelectorScriptSelector2() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.appendSelector(new ScriptSelector());
    andSelector.addSelector(new SelectSelector());
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{andselect: ScriptSelector, , ScriptSelector}", andSelector.toString());
  }

  /**
   * Test {@link BaseSelectorContainer#validate()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor) addNot {@link NotSelector#NotSelector()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#validate()}
   */
  @Test
  public void testValidate_givenAndSelectorAddNotNotSelector_thenThrowBuildException() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.addNot(new NotSelector());
    andSelector.appendSelector(new ScriptSelector());

    // Act and Assert
    assertThrows(BuildException.class, () -> andSelector.validate());
  }

  /**
   * Test {@link BaseSelectorContainer#validate()}.
   * <ul>
   *   <li>Given {@link AndSelector} (default constructor) Error is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseSelectorContainer#validate()}
   */
  @Test
  public void testValidate_givenAndSelectorErrorIsFoo_thenThrowBuildException() {
    // Arrange
    AndSelector andSelector = new AndSelector();
    andSelector.setError("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> andSelector.validate());
  }
}
