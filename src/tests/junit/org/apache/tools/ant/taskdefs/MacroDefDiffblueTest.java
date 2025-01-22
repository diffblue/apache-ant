package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.List;
import java.util.Map;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.MacroDef.Attribute;
import org.apache.tools.ant.taskdefs.MacroDef.NestedSequential;
import org.apache.tools.ant.taskdefs.MacroDef.TemplateElement;
import org.apache.tools.ant.taskdefs.MacroDef.Text;
import org.junit.Test;

public class MacroDefDiffblueTest {
  /**
   * Test {@link MacroDef#addConfiguredText(Text)}.
   * <ul>
   *   <li>Given {@link Attribute} (default constructor) Name is {@code 42}.</li>
   *   <li>Then {@link MacroDef} (default constructor) Text is {@link Text} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredText(Text)}
   */
  @Test
  public void testAddConfiguredText_givenAttributeNameIs42_thenMacroDefTextIsText() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName("42");

    MacroDef macroDef = new MacroDef();
    macroDef.addConfiguredAttribute(attribute);

    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    // Act
    macroDef.addConfiguredText(text);

    // Assert
    assertSame(text, macroDef.getText());
  }

  /**
   * Test {@link MacroDef#addConfiguredText(Text)}.
   * <ul>
   *   <li>Given {@link Attribute} (default constructor) Name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredText(Text)}
   */
  @Test
  public void testAddConfiguredText_givenAttributeNameIsAttribute_name_thenThrowBuildException() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    MacroDef macroDef = new MacroDef();
    macroDef.addConfiguredAttribute(attribute);

    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> macroDef.addConfiguredText(text));
  }

  /**
   * Test {@link MacroDef#addConfiguredText(Text)}.
   * <ul>
   *   <li>Given {@link MacroDef} (default constructor).</li>
   *   <li>Then {@link MacroDef} (default constructor) Text is {@link Text} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredText(Text)}
   */
  @Test
  public void testAddConfiguredText_givenMacroDef_thenMacroDefTextIsText() {
    // Arrange
    MacroDef macroDef = new MacroDef();

    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    // Act
    macroDef.addConfiguredText(text);

    // Assert
    assertSame(text, macroDef.getText());
  }

  /**
   * Test {@link MacroDef#addConfiguredText(Text)}.
   * <ul>
   *   <li>Given {@link Text} (default constructor) Default is {@code Default String}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredText(Text)}
   */
  @Test
  public void testAddConfiguredText_givenTextDefaultIsDefaultString_thenThrowBuildException() {
    // Arrange
    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    MacroDef macroDef = new MacroDef();
    macroDef.addConfiguredText(text);

    Text text2 = new Text();
    text2.setDefault("Default String");
    text2.setDescription("The characteristics of someone or something");
    text2.setName(Manifest.ATTRIBUTE_NAME);
    text2.setOptional(true);
    text2.setTrim(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> macroDef.addConfiguredText(text2));
  }

  /**
   * Test {@link MacroDef#addConfiguredAttribute(Attribute)}.
   * <ul>
   *   <li>Given {@link Attribute} (default constructor) Name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testAddConfiguredAttribute_givenAttributeNameIsAttribute_name() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    MacroDef macroDef = new MacroDef();
    macroDef.addConfiguredAttribute(attribute);

    Attribute attribute2 = new Attribute();
    attribute2.setDefault("42");
    attribute2.setDescription("The characteristics of someone or something");
    attribute2.setDoubleExpanding(true);
    attribute2.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertThrows(BuildException.class, () -> macroDef.addConfiguredAttribute(attribute2));
  }

  /**
   * Test {@link MacroDef#addConfiguredAttribute(Attribute)}.
   * <ul>
   *   <li>Given {@link MacroDef} (default constructor).</li>
   *   <li>Then {@link MacroDef} (default constructor) Attributes size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testAddConfiguredAttribute_givenMacroDef_thenMacroDefAttributesSizeIsOne() {
    // Arrange
    MacroDef macroDef = new MacroDef();

    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    // Act
    macroDef.addConfiguredAttribute(attribute);

    // Assert
    List<Attribute> attributes = macroDef.getAttributes();
    assertEquals(1, attributes.size());
    assertSame(attribute, attributes.get(0));
  }

  /**
   * Test {@link MacroDef#addConfiguredAttribute(Attribute)}.
   * <ul>
   *   <li>Given {@link Text} (default constructor) Default is {@code Default String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testAddConfiguredAttribute_givenTextDefaultIsDefaultString() {
    // Arrange
    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    MacroDef macroDef = new MacroDef();
    macroDef.addConfiguredText(text);

    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertThrows(BuildException.class, () -> macroDef.addConfiguredAttribute(attribute));
  }

  /**
   * Test {@link MacroDef#addConfiguredAttribute(Attribute)}.
   * <ul>
   *   <li>Then {@link MacroDef} (default constructor) Attributes size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testAddConfiguredAttribute_thenMacroDefAttributesSizeIsTwo() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName("42");

    MacroDef macroDef = new MacroDef();
    macroDef.addConfiguredAttribute(attribute);

    Attribute attribute2 = new Attribute();
    attribute2.setDefault("42");
    attribute2.setDescription("The characteristics of someone or something");
    attribute2.setDoubleExpanding(true);
    attribute2.setName(Manifest.ATTRIBUTE_NAME);

    // Act
    macroDef.addConfiguredAttribute(attribute2);

    // Assert
    List<Attribute> attributes = macroDef.getAttributes();
    assertEquals(2, attributes.size());
    assertSame(attribute, attributes.get(0));
    assertSame(attribute2, attributes.get(1));
  }

  /**
   * Test {@link MacroDef#addConfiguredElement(TemplateElement)}.
   * <ul>
   *   <li>Given {@code false}.</li>
   *   <li>When {@link TemplateElement} (default constructor) Implicit is {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredElement(TemplateElement)}
   */
  @Test
  public void testAddConfiguredElement_givenFalse_whenTemplateElementImplicitIsFalse() {
    // Arrange
    MacroDef macroDef = new MacroDef();

    TemplateElement element = new TemplateElement();
    element.setDescription("The characteristics of someone or something");
    element.setImplicit(false);
    element.setName(Manifest.ATTRIBUTE_NAME);
    element.setOptional(true);

    // Act
    macroDef.addConfiguredElement(element);

    // Assert
    Map<String, TemplateElement> elements = macroDef.getElements();
    assertEquals(1, elements.size());
    assertSame(element, elements.get("name"));
  }

  /**
   * Test {@link MacroDef#addConfiguredElement(TemplateElement)}.
   * <ul>
   *   <li>Given {@link MacroDef} (default constructor).</li>
   *   <li>Then {@link MacroDef} (default constructor) Elements size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredElement(TemplateElement)}
   */
  @Test
  public void testAddConfiguredElement_givenMacroDef_thenMacroDefElementsSizeIsOne() {
    // Arrange
    MacroDef macroDef = new MacroDef();

    TemplateElement element = new TemplateElement();
    element.setDescription("The characteristics of someone or something");
    element.setImplicit(true);
    element.setName(Manifest.ATTRIBUTE_NAME);
    element.setOptional(true);

    // Act
    macroDef.addConfiguredElement(element);

    // Assert
    Map<String, TemplateElement> elements = macroDef.getElements();
    assertEquals(1, elements.size());
    assertSame(element, elements.get("name"));
  }

  /**
   * Test {@link MacroDef#addConfiguredElement(TemplateElement)}.
   * <ul>
   *   <li>Given {@link TemplateElement} (default constructor) Implicit is {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredElement(TemplateElement)}
   */
  @Test
  public void testAddConfiguredElement_givenTemplateElementImplicitIsFalse() {
    // Arrange
    TemplateElement element = new TemplateElement();
    element.setDescription("The characteristics of someone or something");
    element.setImplicit(false);
    element.setName("42");
    element.setOptional(true);

    MacroDef macroDef = new MacroDef();
    macroDef.addConfiguredElement(element);

    TemplateElement element2 = new TemplateElement();
    element2.setDescription("The characteristics of someone or something");
    element2.setImplicit(true);
    element2.setName(Manifest.ATTRIBUTE_NAME);
    element2.setOptional(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> macroDef.addConfiguredElement(element2));
  }

  /**
   * Test {@link MacroDef#addConfiguredElement(TemplateElement)}.
   * <ul>
   *   <li>Given {@link TemplateElement} (default constructor) Implicit is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredElement(TemplateElement)}
   */
  @Test
  public void testAddConfiguredElement_givenTemplateElementImplicitIsTrue() {
    // Arrange
    TemplateElement element = new TemplateElement();
    element.setDescription("The characteristics of someone or something");
    element.setImplicit(true);
    element.setName("42");
    element.setOptional(true);

    MacroDef macroDef = new MacroDef();
    macroDef.addConfiguredElement(element);

    TemplateElement element2 = new TemplateElement();
    element2.setDescription("The characteristics of someone or something");
    element2.setImplicit(true);
    element2.setName(Manifest.ATTRIBUTE_NAME);
    element2.setOptional(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> macroDef.addConfiguredElement(element2));
  }

  /**
   * Test {@link MacroDef#addConfiguredElement(TemplateElement)}.
   * <ul>
   *   <li>Given {@link TemplateElement} (default constructor) Name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link MacroDef#addConfiguredElement(TemplateElement)}
   */
  @Test
  public void testAddConfiguredElement_givenTemplateElementNameIsAttribute_name() {
    // Arrange
    TemplateElement element = new TemplateElement();
    element.setDescription("The characteristics of someone or something");
    element.setImplicit(true);
    element.setName(Manifest.ATTRIBUTE_NAME);
    element.setOptional(true);

    MacroDef macroDef = new MacroDef();
    macroDef.addConfiguredElement(element);

    TemplateElement element2 = new TemplateElement();
    element2.setDescription("The characteristics of someone or something");
    element2.setImplicit(true);
    element2.setName(Manifest.ATTRIBUTE_NAME);
    element2.setOptional(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> macroDef.addConfiguredElement(element2));
  }

  /**
   * Test Attribute {@link Attribute#equals(Object)}, and {@link Attribute#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Attribute#equals(Object)}
   *   <li>{@link Attribute#hashCode()}
   * </ul>
   */
  @Test
  public void testAttributeEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    Attribute attribute2 = new Attribute();
    attribute2.setDefault("42");
    attribute2.setDescription("The characteristics of someone or something");
    attribute2.setDoubleExpanding(true);
    attribute2.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertEquals(attribute, attribute2);
    int expectedHashCodeResult = attribute.hashCode();
    assertEquals(expectedHashCodeResult, attribute2.hashCode());
  }

  /**
   * Test Attribute {@link Attribute#equals(Object)}, and {@link Attribute#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Attribute#equals(Object)}
   *   <li>{@link Attribute#hashCode()}
   * </ul>
   */
  @Test
  public void testAttributeEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual2() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault(null);
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    Attribute attribute2 = new Attribute();
    attribute2.setDefault(null);
    attribute2.setDescription("The characteristics of someone or something");
    attribute2.setDoubleExpanding(true);
    attribute2.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertEquals(attribute, attribute2);
    int expectedHashCodeResult = attribute.hashCode();
    assertEquals(expectedHashCodeResult, attribute2.hashCode());
  }

  /**
   * Test Attribute {@link Attribute#equals(Object)}, and {@link Attribute#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Attribute#equals(Object)}
   *   <li>{@link Attribute#hashCode()}
   * </ul>
   */
  @Test
  public void testAttributeEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertEquals(attribute, attribute);
    int expectedHashCodeResult = attribute.hashCode();
    assertEquals(expectedHashCodeResult, attribute.hashCode());
  }

  /**
   * Test Attribute {@link Attribute#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#equals(Object)}
   */
  @Test
  public void testAttributeEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("name");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    Attribute attribute2 = new Attribute();
    attribute2.setDefault("42");
    attribute2.setDescription("The characteristics of someone or something");
    attribute2.setDoubleExpanding(true);
    attribute2.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertNotEquals(attribute, attribute2);
  }

  /**
   * Test Attribute {@link Attribute#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#equals(Object)}
   */
  @Test
  public void testAttributeEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault(null);
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    Attribute attribute2 = new Attribute();
    attribute2.setDefault("42");
    attribute2.setDescription("The characteristics of someone or something");
    attribute2.setDoubleExpanding(true);
    attribute2.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertNotEquals(attribute, attribute2);
  }

  /**
   * Test Attribute {@link Attribute#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#equals(Object)}
   */
  @Test
  public void testAttributeEquals_whenOtherIsDifferent_thenReturnNotEqual3() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName("42");

    Attribute attribute2 = new Attribute();
    attribute2.setDefault("42");
    attribute2.setDescription("The characteristics of someone or something");
    attribute2.setDoubleExpanding(true);
    attribute2.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertNotEquals(attribute, attribute2);
  }

  /**
   * Test Attribute {@link Attribute#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#equals(Object)}
   */
  @Test
  public void testAttributeEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertNotEquals(attribute, null);
  }

  /**
   * Test Attribute {@link Attribute#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#equals(Object)}
   */
  @Test
  public void testAttributeEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertNotEquals(attribute, "Different type to Attribute");
  }

  /**
   * Test Attribute getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Attribute}
   *   <li>{@link Attribute#setDefault(String)}
   *   <li>{@link Attribute#setDescription(String)}
   *   <li>{@link Attribute#setDoubleExpanding(boolean)}
   *   <li>{@link Attribute#getDefault()}
   *   <li>{@link Attribute#getDescription()}
   *   <li>{@link Attribute#getName()}
   *   <li>{@link Attribute#isDoubleExpanding()}
   * </ul>
   */
  @Test
  public void testAttributeGettersAndSetters() {
    // Arrange and Act
    Attribute actualAttribute = new Attribute();
    actualAttribute.setDefault("42");
    actualAttribute.setDescription("The characteristics of someone or something");
    actualAttribute.setDoubleExpanding(true);
    String actualDefault = actualAttribute.getDefault();
    String actualDescription = actualAttribute.getDescription();
    String actualName = actualAttribute.getName();

    // Assert
    assertEquals("42", actualDefault);
    assertEquals("The characteristics of someone or something", actualDescription);
    assertNull(actualName);
    assertTrue(actualAttribute.isDoubleExpanding());
  }

  /**
   * Test Attribute {@link Attribute#setName(String)}.
   * <ul>
   *   <li>Given {@link Attribute} (default constructor).</li>
   *   <li>When {@link Manifest#ATTRIBUTE_NAME}.</li>
   *   <li>Then {@link Attribute} (default constructor) Name is {@code name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#setName(String)}
   */
  @Test
  public void testAttributeSetName_givenAttribute_whenAttribute_name_thenAttributeNameIsName() {
    // Arrange
    Attribute attribute = new Attribute();

    // Act
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    // Assert
    assertEquals("name", attribute.getName());
  }

  /**
   * Test Attribute {@link Attribute#setName(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#setName(String)}
   */
  @Test
  public void testAttributeSetName_whenEmptyString_thenThrowBuildException() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertThrows(BuildException.class, () -> attribute.setName(""));
  }

  /**
   * Test Attribute {@link Attribute#setName(String)}.
   * <ul>
   *   <li>When {@code Illegal name [%s] for attribute}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#setName(String)}
   */
  @Test
  public void testAttributeSetName_whenIllegalNameSForAttribute_thenThrowBuildException() {
    // Arrange
    Attribute attribute = new Attribute();
    attribute.setDefault("42");
    attribute.setDescription("The characteristics of someone or something");
    attribute.setDoubleExpanding(true);
    attribute.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertThrows(BuildException.class, () -> attribute.setName("Illegal name [%s] for attribute"));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link MacroDef}
   *   <li>{@link MacroDef#setBackTrace(boolean)}
   *   <li>{@link MacroDef#setName(String)}
   *   <li>{@link MacroDef#getAttributes()}
   *   <li>{@link MacroDef#getBackTrace()}
   *   <li>{@link MacroDef#getElements()}
   *   <li>{@link MacroDef#getText()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    MacroDef actualMacroDef = new MacroDef();
    actualMacroDef.setBackTrace(true);
    actualMacroDef.setName(Manifest.ATTRIBUTE_NAME);
    List<Attribute> actualAttributes = actualMacroDef.getAttributes();
    boolean actualBackTrace = actualMacroDef.getBackTrace();
    Map<String, TemplateElement> actualElements = actualMacroDef.getElements();
    Text actualText = actualMacroDef.getText();

    // Assert
    assertEquals("", actualMacroDef.getURI());
    assertNull(actualMacroDef.getAntlibClassLoader());
    Location location = actualMacroDef.getLocation();
    assertNull(location.getFileName());
    assertNull(actualMacroDef.getDescription());
    assertNull(actualMacroDef.getTaskName());
    assertNull(actualMacroDef.getTaskType());
    assertNull(actualMacroDef.getProject());
    assertNull(actualMacroDef.getOwningTarget());
    assertNull(actualText);
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(actualAttributes.isEmpty());
    assertTrue(actualElements.isEmpty());
    assertTrue(actualBackTrace);
  }

  /**
   * Test NestedSequential {@link NestedSequential#addTask(Task)}.
   * <p>
   * Method under test: {@link NestedSequential#addTask(Task)}
   */
  @Test
  public void testNestedSequentialAddTask() {
    // Arrange
    NestedSequential nestedSequential = new NestedSequential();
    TaskAdapter task = new TaskAdapter();

    // Act
    nestedSequential.addTask(task);

    // Assert
    List<Task> nested = nestedSequential.getNested();
    assertEquals(1, nested.size());
    assertSame(task, nested.get(0));
  }

  /**
   * Test NestedSequential getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link NestedSequential}
   *   <li>{@link NestedSequential#getNested()}
   * </ul>
   */
  @Test
  public void testNestedSequentialGettersAndSetters() {
    // Arrange, Act and Assert
    assertTrue((new NestedSequential()).getNested().isEmpty());
  }

  /**
   * Test NestedSequential {@link NestedSequential#similar(NestedSequential)}.
   * <ul>
   *   <li>Given {@link NestedSequential} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NestedSequential#similar(NestedSequential)}
   */
  @Test
  public void testNestedSequentialSimilar_givenNestedSequential_thenReturnTrue() {
    // Arrange
    NestedSequential nestedSequential = new NestedSequential();

    // Act and Assert
    assertTrue(nestedSequential.similar(new NestedSequential()));
  }

  /**
   * Test NestedSequential {@link NestedSequential#similar(NestedSequential)}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link NestedSequential#similar(NestedSequential)}
   */
  @Test
  public void testNestedSequentialSimilar_thenReturnFalse() {
    // Arrange
    NestedSequential nestedSequential = new NestedSequential();
    nestedSequential.addTask(new TaskAdapter());

    // Act and Assert
    assertFalse(nestedSequential.similar(new NestedSequential()));
  }

  /**
   * Test TemplateElement {@link TemplateElement#equals(Object)}, and {@link TemplateElement#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TemplateElement#equals(Object)}
   *   <li>{@link TemplateElement#hashCode()}
   * </ul>
   */
  @Test
  public void testTemplateElementEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    TemplateElement templateElement = new TemplateElement();
    templateElement.setDescription("The characteristics of someone or something");
    templateElement.setImplicit(true);
    templateElement.setName(Manifest.ATTRIBUTE_NAME);
    templateElement.setOptional(true);

    TemplateElement templateElement2 = new TemplateElement();
    templateElement2.setDescription("The characteristics of someone or something");
    templateElement2.setImplicit(true);
    templateElement2.setName(Manifest.ATTRIBUTE_NAME);
    templateElement2.setOptional(true);

    // Act and Assert
    assertEquals(templateElement, templateElement2);
    int expectedHashCodeResult = templateElement.hashCode();
    assertEquals(expectedHashCodeResult, templateElement2.hashCode());
  }

  /**
   * Test TemplateElement {@link TemplateElement#equals(Object)}, and {@link TemplateElement#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TemplateElement#equals(Object)}
   *   <li>{@link TemplateElement#hashCode()}
   * </ul>
   */
  @Test
  public void testTemplateElementEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    TemplateElement templateElement = new TemplateElement();
    templateElement.setDescription("The characteristics of someone or something");
    templateElement.setImplicit(true);
    templateElement.setName(Manifest.ATTRIBUTE_NAME);
    templateElement.setOptional(true);

    // Act and Assert
    assertEquals(templateElement, templateElement);
    int expectedHashCodeResult = templateElement.hashCode();
    assertEquals(expectedHashCodeResult, templateElement.hashCode());
  }

  /**
   * Test TemplateElement {@link TemplateElement#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TemplateElement#equals(Object)}
   */
  @Test
  public void testTemplateElementEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    TemplateElement templateElement = new TemplateElement();
    templateElement.setDescription("The characteristics of someone or something");
    templateElement.setImplicit(false);
    templateElement.setName(Manifest.ATTRIBUTE_NAME);
    templateElement.setOptional(true);

    TemplateElement templateElement2 = new TemplateElement();
    templateElement2.setDescription("The characteristics of someone or something");
    templateElement2.setImplicit(true);
    templateElement2.setName(Manifest.ATTRIBUTE_NAME);
    templateElement2.setOptional(true);

    // Act and Assert
    assertNotEquals(templateElement, templateElement2);
  }

  /**
   * Test TemplateElement {@link TemplateElement#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TemplateElement#equals(Object)}
   */
  @Test
  public void testTemplateElementEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    TemplateElement templateElement = new TemplateElement();
    templateElement.setDescription("The characteristics of someone or something");
    templateElement.setImplicit(true);
    templateElement.setName("42");
    templateElement.setOptional(true);

    TemplateElement templateElement2 = new TemplateElement();
    templateElement2.setDescription("The characteristics of someone or something");
    templateElement2.setImplicit(true);
    templateElement2.setName(Manifest.ATTRIBUTE_NAME);
    templateElement2.setOptional(true);

    // Act and Assert
    assertNotEquals(templateElement, templateElement2);
  }

  /**
   * Test TemplateElement {@link TemplateElement#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TemplateElement#equals(Object)}
   */
  @Test
  public void testTemplateElementEquals_whenOtherIsDifferent_thenReturnNotEqual3() {
    // Arrange
    TemplateElement templateElement = new TemplateElement();
    templateElement.setDescription("The characteristics of someone or something");
    templateElement.setImplicit(true);
    templateElement.setName(Manifest.ATTRIBUTE_NAME);
    templateElement.setOptional(false);

    TemplateElement templateElement2 = new TemplateElement();
    templateElement2.setDescription("The characteristics of someone or something");
    templateElement2.setImplicit(true);
    templateElement2.setName(Manifest.ATTRIBUTE_NAME);
    templateElement2.setOptional(true);

    // Act and Assert
    assertNotEquals(templateElement, templateElement2);
  }

  /**
   * Test TemplateElement {@link TemplateElement#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TemplateElement#equals(Object)}
   */
  @Test
  public void testTemplateElementEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange
    TemplateElement templateElement = new TemplateElement();
    templateElement.setDescription("The characteristics of someone or something");
    templateElement.setImplicit(true);
    templateElement.setName(Manifest.ATTRIBUTE_NAME);
    templateElement.setOptional(true);

    // Act and Assert
    assertNotEquals(templateElement, null);
  }

  /**
   * Test TemplateElement {@link TemplateElement#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link TemplateElement#equals(Object)}
   */
  @Test
  public void testTemplateElementEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange
    TemplateElement templateElement = new TemplateElement();
    templateElement.setDescription("The characteristics of someone or something");
    templateElement.setImplicit(true);
    templateElement.setName(Manifest.ATTRIBUTE_NAME);
    templateElement.setOptional(true);

    // Act and Assert
    assertNotEquals(templateElement, "Different type to TemplateElement");
  }

  /**
   * Test TemplateElement getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link TemplateElement}
   *   <li>{@link TemplateElement#setDescription(String)}
   *   <li>{@link TemplateElement#setImplicit(boolean)}
   *   <li>{@link TemplateElement#setOptional(boolean)}
   *   <li>{@link TemplateElement#getDescription()}
   *   <li>{@link TemplateElement#getName()}
   *   <li>{@link TemplateElement#isImplicit()}
   *   <li>{@link TemplateElement#isOptional()}
   * </ul>
   */
  @Test
  public void testTemplateElementGettersAndSetters() {
    // Arrange and Act
    TemplateElement actualTemplateElement = new TemplateElement();
    actualTemplateElement.setDescription("The characteristics of someone or something");
    actualTemplateElement.setImplicit(true);
    actualTemplateElement.setOptional(true);
    String actualDescription = actualTemplateElement.getDescription();
    String actualName = actualTemplateElement.getName();
    boolean actualIsImplicitResult = actualTemplateElement.isImplicit();

    // Assert
    assertEquals("The characteristics of someone or something", actualDescription);
    assertNull(actualName);
    assertTrue(actualIsImplicitResult);
    assertTrue(actualTemplateElement.isOptional());
  }

  /**
   * Test TemplateElement {@link TemplateElement#setName(String)}.
   * <ul>
   *   <li>Given {@link TemplateElement} (default constructor).</li>
   *   <li>Then {@link TemplateElement} (default constructor) Name is {@code name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TemplateElement#setName(String)}
   */
  @Test
  public void testTemplateElementSetName_givenTemplateElement_thenTemplateElementNameIsName() {
    // Arrange
    TemplateElement templateElement = new TemplateElement();

    // Act
    templateElement.setName(Manifest.ATTRIBUTE_NAME);

    // Assert
    assertEquals("name", templateElement.getName());
  }

  /**
   * Test TemplateElement {@link TemplateElement#setName(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TemplateElement#setName(String)}
   */
  @Test
  public void testTemplateElementSetName_whenEmptyString_thenThrowBuildException() {
    // Arrange
    TemplateElement templateElement = new TemplateElement();
    templateElement.setDescription("The characteristics of someone or something");
    templateElement.setImplicit(true);
    templateElement.setName(Manifest.ATTRIBUTE_NAME);
    templateElement.setOptional(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> templateElement.setName(""));
  }

  /**
   * Test TemplateElement {@link TemplateElement#setName(String)}.
   * <ul>
   *   <li>When {@code Illegal name [%s] for macro element}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TemplateElement#setName(String)}
   */
  @Test
  public void testTemplateElementSetName_whenIllegalNameSForMacroElement() {
    // Arrange
    TemplateElement templateElement = new TemplateElement();
    templateElement.setDescription("The characteristics of someone or something");
    templateElement.setImplicit(true);
    templateElement.setName(Manifest.ATTRIBUTE_NAME);
    templateElement.setOptional(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> templateElement.setName("Illegal name [%s] for macro element"));
  }

  /**
   * Test Text {@link Text#equals(Object)}, and {@link Text#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Text#equals(Object)}
   *   <li>{@link Text#hashCode()}
   * </ul>
   */
  @Test
  public void testTextEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    Text text2 = new Text();
    text2.setDefault("Default String");
    text2.setDescription("The characteristics of someone or something");
    text2.setName(Manifest.ATTRIBUTE_NAME);
    text2.setOptional(true);
    text2.setTrim(true);

    // Act and Assert
    assertEquals(text, text2);
    int expectedHashCodeResult = text.hashCode();
    assertEquals(expectedHashCodeResult, text2.hashCode());
  }

  /**
   * Test Text {@link Text#equals(Object)}, and {@link Text#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Text#equals(Object)}
   *   <li>{@link Text#hashCode()}
   * </ul>
   */
  @Test
  public void testTextEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    // Act and Assert
    assertEquals(text, text);
    int expectedHashCodeResult = text.hashCode();
    assertEquals(expectedHashCodeResult, text.hashCode());
  }

  /**
   * Test Text {@link Text#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#equals(Object)}
   */
  @Test
  public void testTextEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    Text text = new Text();
    text.setDefault(null);
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    Text text2 = new Text();
    text2.setDefault("Default String");
    text2.setDescription("The characteristics of someone or something");
    text2.setName(Manifest.ATTRIBUTE_NAME);
    text2.setOptional(true);
    text2.setTrim(true);

    // Act and Assert
    assertNotEquals(text, text2);
  }

  /**
   * Test Text {@link Text#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#equals(Object)}
   */
  @Test
  public void testTextEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName("42");
    text.setOptional(true);
    text.setTrim(true);

    Text text2 = new Text();
    text2.setDefault("Default String");
    text2.setDescription("The characteristics of someone or something");
    text2.setName(Manifest.ATTRIBUTE_NAME);
    text2.setOptional(true);
    text2.setTrim(true);

    // Act and Assert
    assertNotEquals(text, text2);
  }

  /**
   * Test Text {@link Text#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#equals(Object)}
   */
  @Test
  public void testTextEquals_whenOtherIsDifferent_thenReturnNotEqual3() {
    // Arrange
    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(false);
    text.setTrim(true);

    Text text2 = new Text();
    text2.setDefault("Default String");
    text2.setDescription("The characteristics of someone or something");
    text2.setName(Manifest.ATTRIBUTE_NAME);
    text2.setOptional(true);
    text2.setTrim(true);

    // Act and Assert
    assertNotEquals(text, text2);
  }

  /**
   * Test Text {@link Text#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#equals(Object)}
   */
  @Test
  public void testTextEquals_whenOtherIsDifferent_thenReturnNotEqual4() {
    // Arrange
    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(false);

    Text text2 = new Text();
    text2.setDefault("Default String");
    text2.setDescription("The characteristics of someone or something");
    text2.setName(Manifest.ATTRIBUTE_NAME);
    text2.setOptional(true);
    text2.setTrim(true);

    // Act and Assert
    assertNotEquals(text, text2);
  }

  /**
   * Test Text {@link Text#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#equals(Object)}
   */
  @Test
  public void testTextEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange
    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    // Act and Assert
    assertNotEquals(text, null);
  }

  /**
   * Test Text {@link Text#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#equals(Object)}
   */
  @Test
  public void testTextEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange
    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    // Act and Assert
    assertNotEquals(text, "Different type to Text");
  }

  /**
   * Test Text getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Text}
   *   <li>{@link Text#setDefault(String)}
   *   <li>{@link Text#setDescription(String)}
   *   <li>{@link Text#setOptional(boolean)}
   *   <li>{@link Text#setTrim(boolean)}
   *   <li>{@link Text#getDefault()}
   *   <li>{@link Text#getDescription()}
   *   <li>{@link Text#getName()}
   *   <li>{@link Text#getOptional()}
   *   <li>{@link Text#getTrim()}
   * </ul>
   */
  @Test
  public void testTextGettersAndSetters() {
    // Arrange and Act
    Text actualText = new Text();
    actualText.setDefault("Default String");
    actualText.setDescription("The characteristics of someone or something");
    actualText.setOptional(true);
    actualText.setTrim(true);
    String actualDefault = actualText.getDefault();
    String actualDescription = actualText.getDescription();
    String actualName = actualText.getName();
    boolean actualOptional = actualText.getOptional();

    // Assert
    assertEquals("Default String", actualDefault);
    assertEquals("The characteristics of someone or something", actualDescription);
    assertNull(actualName);
    assertTrue(actualOptional);
    assertTrue(actualText.getTrim());
  }

  /**
   * Test Text {@link Text#setName(String)}.
   * <ul>
   *   <li>Given {@link Text} (default constructor) Default is {@code Default String}.</li>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#setName(String)}
   */
  @Test
  public void testTextSetName_givenTextDefaultIsDefaultString_whenEmptyString() {
    // Arrange
    Text text = new Text();
    text.setDefault("Default String");
    text.setDescription("The characteristics of someone or something");
    text.setName(Manifest.ATTRIBUTE_NAME);
    text.setOptional(true);
    text.setTrim(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> text.setName(""));
  }

  /**
   * Test Text {@link Text#setName(String)}.
   * <ul>
   *   <li>Given {@link Text} (default constructor).</li>
   *   <li>When {@link Manifest#ATTRIBUTE_NAME}.</li>
   *   <li>Then {@link Text} (default constructor) Name is {@code name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#setName(String)}
   */
  @Test
  public void testTextSetName_givenText_whenAttribute_name_thenTextNameIsName() {
    // Arrange
    Text text = new Text();

    // Act
    text.setName(Manifest.ATTRIBUTE_NAME);

    // Assert
    assertEquals("name", text.getName());
  }

  /**
   * Test Text {@link Text#setName(String)}.
   * <ul>
   *   <li>When {@code MacroDef$Text}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Text#setName(String)}
   */
  @Test
  public void testTextSetName_whenOrgApacheToolsAntTaskdefsMacroDefText() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Text()).setName("org.apache.tools.ant.taskdefs.MacroDef$Text"));
  }
}
