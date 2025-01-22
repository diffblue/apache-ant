package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.condition.IsFileSelected;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.junit.Test;

public class AbstractSelectorContainerDiffblueTest {
  /**
   * Test {@link AbstractSelectorContainer#hasSelectors()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor) appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#hasSelectors()}
   */
  @Test
  public void testHasSelectors_givenIsFileSelectedAppendSelectorScriptSelector_thenReturnTrue() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.appendSelector(new ScriptSelector());

    // Act and Assert
    assertTrue(isFileSelected.hasSelectors());
  }

  /**
   * Test {@link AbstractSelectorContainer#hasSelectors()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#hasSelectors()}
   */
  @Test
  public void testHasSelectors_givenIsFileSelected_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new IsFileSelected()).hasSelectors());
  }

  /**
   * Test {@link AbstractSelectorContainer#selectorCount()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor) appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#selectorCount()}
   */
  @Test
  public void testSelectorCount_givenIsFileSelectedAppendSelectorScriptSelector_thenReturnOne() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals(1, isFileSelected.selectorCount());
  }

  /**
   * Test {@link AbstractSelectorContainer#selectorCount()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#selectorCount()}
   */
  @Test
  public void testSelectorCount_givenIsFileSelected_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new IsFileSelected()).selectorCount());
  }

  /**
   * Test {@link AbstractSelectorContainer#getSelectors(Project)}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor).</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#getSelectors(Project)}
   */
  @Test
  public void testGetSelectors_givenIsFileSelected_thenReturnArrayLengthIsZero() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();

    // Act and Assert
    assertEquals(0, isFileSelected.getSelectors(new Project()).length);
  }

  /**
   * Test {@link AbstractSelectorContainer#getSelectors(Project)}.
   * <ul>
   *   <li>Then return array length is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#getSelectors(Project)}
   */
  @Test
  public void testGetSelectors_thenReturnArrayLengthIsOne() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();
    ScriptSelector selector = new ScriptSelector();
    isFileSelected.appendSelector(selector);

    // Act
    FileSelector[] actualSelectors = isFileSelected.getSelectors(new Project());

    // Assert
    assertEquals(1, actualSelectors.length);
    assertSame(selector, actualSelectors[0]);
  }

  /**
   * Test {@link AbstractSelectorContainer#toString()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor) addAnd {@link AndSelector} (default constructor).</li>
   *   <li>Then return {@code , ScriptSelector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#toString()}
   */
  @Test
  public void testToString_givenIsFileSelectedAddAndAndSelector_thenReturnScriptSelector() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.addAnd(new AndSelector());
    isFileSelected.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals(", ScriptSelector", isFileSelected.toString());
  }

  /**
   * Test {@link AbstractSelectorContainer#toString()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#toString()}
   */
  @Test
  public void testToString_givenIsFileSelected_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new IsFileSelected()).toString());
  }

  /**
   * Test {@link AbstractSelectorContainer#toString()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code ScriptSelector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#toString()}
   */
  @Test
  public void testToString_givenJavaLangObject_thenReturnScriptSelector() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(", ", typeClass);

    ScriptSelector selector = new ScriptSelector();
    selector.setProject(project);

    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.appendSelector(selector);

    // Act and Assert
    assertEquals("ScriptSelector", isFileSelected.toString());
  }

  /**
   * Test {@link AbstractSelectorContainer#toString()}.
   * <ul>
   *   <li>Given {@link ScriptSelector} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code ScriptSelector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#toString()}
   */
  @Test
  public void testToString_givenScriptSelectorProjectIsProject_thenReturnScriptSelector() {
    // Arrange
    ScriptSelector selector = new ScriptSelector();
    selector.setProject(new Project());

    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.appendSelector(selector);

    // Act and Assert
    assertEquals("ScriptSelector", isFileSelected.toString());
  }

  /**
   * Test {@link AbstractSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code ScriptSelector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnScriptSelector() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("ScriptSelector", isFileSelected.toString());
  }

  /**
   * Test {@link AbstractSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code , ScriptSelector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnScriptSelector2() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.addSelector(new SelectSelector());
    isFileSelected.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals(", ScriptSelector", isFileSelected.toString());
  }

  /**
   * Test {@link AbstractSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code ScriptSelector, ScriptSelector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnScriptSelectorScriptSelector() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.appendSelector(new ScriptSelector());
    isFileSelected.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("ScriptSelector, ScriptSelector", isFileSelected.toString());
  }

  /**
   * Test {@link AbstractSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code ScriptSelector, , ScriptSelector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnScriptSelectorScriptSelector2() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.appendSelector(new ScriptSelector());
    isFileSelected.addSelector(new SelectSelector());
    isFileSelected.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("ScriptSelector, , ScriptSelector", isFileSelected.toString());
  }

  /**
   * Test {@link AbstractSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code ScriptSelector The characteristics of someone or something}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnScriptSelectorTheCharacteristicsOfSomeoneOrSomething() {
    // Arrange
    ScriptSelector selector = new ScriptSelector();
    selector.setDescription("The characteristics of someone or something");

    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.appendSelector(selector);

    // Act and Assert
    assertEquals("ScriptSelector The characteristics of someone or something", isFileSelected.toString());
  }

  /**
   * Test {@link AbstractSelectorContainer#toString()}.
   * <ul>
   *   <li>Then return {@code {select ScriptSelector}, ScriptSelector}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#toString()}
   */
  @Test
  public void testToString_thenReturnSelectScriptSelectorScriptSelector() {
    // Arrange
    SelectSelector selector = new SelectSelector();
    selector.appendSelector(new ScriptSelector());

    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.addSelector(selector);
    isFileSelected.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("{select ScriptSelector}, ScriptSelector", isFileSelected.toString());
  }

  /**
   * Test {@link AbstractSelectorContainer#clone()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#clone()}
   */
  @Test
  public void testClone_givenIsFileSelected() {
    // Arrange and Act
    Object actualCloneResult = (new IsFileSelected()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof IsFileSelected);
    Location location = ((IsFileSelected) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((IsFileSelected) actualCloneResult).getDescription());
    assertNull(((IsFileSelected) actualCloneResult).getProject());
    assertNull(((IsFileSelected) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(((IsFileSelected) actualCloneResult).isReference());
  }

  /**
   * Test {@link AbstractSelectorContainer#clone()}.
   * <ul>
   *   <li>Given {@link IsFileSelected} (default constructor) appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractSelectorContainer#clone()}
   */
  @Test
  public void testClone_givenIsFileSelectedAppendSelectorScriptSelector() {
    // Arrange
    IsFileSelected isFileSelected = new IsFileSelected();
    isFileSelected.appendSelector(new ScriptSelector());

    // Act
    Object actualCloneResult = isFileSelected.clone();

    // Assert
    assertTrue(actualCloneResult instanceof IsFileSelected);
    Location location = ((IsFileSelected) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((IsFileSelected) actualCloneResult).getDescription());
    assertNull(((IsFileSelected) actualCloneResult).getProject());
    assertNull(((IsFileSelected) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(((IsFileSelected) actualCloneResult).isReference());
  }
}
