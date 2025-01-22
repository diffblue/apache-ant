package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.BufferedReader;
import java.io.CharArrayReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.Manifest.Attribute;
import org.apache.tools.ant.taskdefs.Manifest.Section;
import org.junit.Test;

public class ManifestDiffblueTest {
  /**
   * Test Attribute {@link Attribute#addContinuation(String)}.
   * <p>
   * Method under test: {@link Attribute#addContinuation(String)}
   */
  @Test
  public void testAttributeAddContinuation() {
    // Arrange
    Attribute attribute = new Attribute(Manifest.ATTRIBUTE_NAME, "42");

    // Act
    attribute.addContinuation("Line");

    // Assert
    assertEquals("42ine", attribute.getValue());
  }

  /**
   * Test Attribute {@link Attribute#addValue(String)}.
   * <ul>
   *   <li>Given {@link Attribute#Attribute()}.</li>
   *   <li>Then {@link Attribute#Attribute()} Value is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#addValue(String)}
   */
  @Test
  public void testAttributeAddValue_givenAttribute_thenAttributeValueIs42() {
    // Arrange
    Attribute attribute = new Attribute();

    // Act
    attribute.addValue("42");

    // Assert
    assertEquals("42", attribute.getValue());
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
    // Arrange, Act and Assert
    assertNotEquals(new Attribute(), "Rhs");
  }

  /**
   * Test Attribute {@link Attribute#getKey()}.
   * <ul>
   *   <li>Given {@link Attribute#Attribute()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#getKey()}
   */
  @Test
  public void testAttributeGetKey_givenAttribute_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Attribute()).getKey());
  }

  /**
   * Test Attribute {@link Attribute#getKey()}.
   * <ul>
   *   <li>Then return {@code name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#getKey()}
   */
  @Test
  public void testAttributeGetKey_thenReturnName() {
    // Arrange, Act and Assert
    assertEquals("name", (new Attribute(Manifest.ATTRIBUTE_NAME, "42")).getKey());
  }

  /**
   * Test Attribute {@link Attribute#getValue()}.
   * <ul>
   *   <li>Given {@link Attribute#Attribute()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#getValue()}
   */
  @Test
  public void testAttributeGetValue_givenAttribute_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Attribute()).getValue());
  }

  /**
   * Test Attribute {@link Attribute#getValue()}.
   * <ul>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#getValue()}
   */
  @Test
  public void testAttributeGetValue_thenReturn42() {
    // Arrange, Act and Assert
    assertEquals("42", (new Attribute(Manifest.ATTRIBUTE_NAME, "42")).getValue());
  }

  /**
   * Test Attribute getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Attribute#Attribute()}
   *   <li>{@link Attribute#setName(String)}
   *   <li>{@link Attribute#getName()}
   * </ul>
   */
  @Test
  public void testAttributeGettersAndSetters() {
    // Arrange and Act
    Attribute actualAttribute = new Attribute();
    actualAttribute.setName(Manifest.ATTRIBUTE_NAME);

    // Assert
    assertEquals(Manifest.ATTRIBUTE_NAME, actualAttribute.getName());
  }

  /**
   * Test Attribute {@link Attribute#Attribute(String, String)}.
   * <p>
   * Method under test: {@link Attribute#Attribute(String, String)}
   */
  @Test
  public void testAttributeNewAttribute() {
    // Arrange and Act
    Attribute actualAttribute = new Attribute(Manifest.ATTRIBUTE_NAME, "42");

    // Assert
    assertEquals("42", actualAttribute.getValue());
    assertEquals("name", actualAttribute.getKey());
    assertEquals(Manifest.ATTRIBUTE_NAME, actualAttribute.getName());
  }

  /**
   * Test Attribute {@link Attribute#Attribute(String)}.
   * <ul>
   *   <li>When {@code :}.</li>
   *   <li>Then return Key is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#Attribute(String)}
   */
  @Test
  public void testAttributeNewAttribute_whenColon_thenReturnKeyIsEmptyString() throws ManifestException {
    // Arrange and Act
    Attribute actualAttribute = new Attribute(": ");

    // Assert
    assertEquals("", actualAttribute.getKey());
    assertEquals("", actualAttribute.getName());
    assertEquals("", actualAttribute.getValue());
  }

  /**
   * Test Attribute {@link Attribute#Attribute(String)}.
   * <ul>
   *   <li>When {@code Line}.</li>
   *   <li>Then throw {@link ManifestException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#Attribute(String)}
   */
  @Test
  public void testAttributeNewAttribute_whenLine_thenThrowManifestException() throws ManifestException {
    // Arrange, Act and Assert
    assertThrows(ManifestException.class, () -> new Attribute("Line"));
  }

  /**
   * Test Attribute {@link Attribute#parse(String)}.
   * <p>
   * Method under test: {@link Attribute#parse(String)}
   */
  @Test
  public void testAttributeParse() throws ManifestException {
    // Arrange
    Attribute attribute = new Attribute(Manifest.ATTRIBUTE_NAME, "42");

    // Act
    attribute.parse(": ");

    // Assert
    assertEquals("", attribute.getKey());
    assertEquals("", attribute.getName());
    assertEquals("", attribute.getValue());
  }

  /**
   * Test Attribute {@link Attribute#parse(String)}.
   * <ul>
   *   <li>Given {@link Attribute#Attribute()}.</li>
   *   <li>When {@code :}.</li>
   *   <li>Then {@link Attribute#Attribute()} Key is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#parse(String)}
   */
  @Test
  public void testAttributeParse_givenAttribute_whenColon_thenAttributeKeyIsEmptyString() throws ManifestException {
    // Arrange
    Attribute attribute = new Attribute();

    // Act
    attribute.parse(": ");

    // Assert
    assertEquals("", attribute.getKey());
    assertEquals("", attribute.getName());
    assertEquals("", attribute.getValue());
  }

  /**
   * Test Attribute {@link Attribute#parse(String)}.
   * <ul>
   *   <li>Given {@link Attribute#Attribute()}.</li>
   *   <li>When {@code Line}.</li>
   *   <li>Then throw {@link ManifestException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#parse(String)}
   */
  @Test
  public void testAttributeParse_givenAttribute_whenLine_thenThrowManifestException() throws ManifestException {
    // Arrange, Act and Assert
    assertThrows(ManifestException.class, () -> (new Attribute()).parse("Line"));
  }

  /**
   * Test Attribute {@link Attribute#setValue(String)}.
   * <ul>
   *   <li>Given {@link Attribute#Attribute()}.</li>
   *   <li>Then {@link Attribute#Attribute()} Value is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#setValue(String)}
   */
  @Test
  public void testAttributeSetValue_givenAttribute_thenAttributeValueIs42() {
    // Arrange
    Attribute attribute = new Attribute();

    // Act
    attribute.setValue("42");

    // Assert
    assertEquals("42", attribute.getValue());
  }

  /**
   * Test Attribute {@link Attribute#setValue(String)}.
   * <ul>
   *   <li>Then {@link Attribute#Attribute(String, String)} with name is {@link Manifest#ATTRIBUTE_NAME} and value is {@code 42} Value is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#setValue(String)}
   */
  @Test
  public void testAttributeSetValue_thenAttributeWithNameIsAttribute_nameAndValueIs42ValueIs42() {
    // Arrange
    Attribute attribute = new Attribute(Manifest.ATTRIBUTE_NAME, "42");

    // Act
    attribute.setValue("42");

    // Assert that nothing has changed
    assertEquals("42", attribute.getValue());
  }

  /**
   * Test {@link Manifest#getDefaultManifest()}.
   * <p>
   * Method under test: {@link Manifest#getDefaultManifest()}
   */
  @Test
  public void testGetDefaultManifest() throws BuildException {
    // Arrange and Act
    Manifest actualDefaultManifest = Manifest.getDefaultManifest();

    // Assert
    assertNull(actualDefaultManifest.getMainSection().getName());
    assertEquals(Manifest.DEFAULT_MANIFEST_VERSION, actualDefaultManifest.getManifestVersion());
  }

  /**
   * Test {@link Manifest#Manifest()}.
   * <p>
   * Method under test: {@link Manifest#Manifest()}
   */
  @Test
  public void testNewManifest() {
    // Arrange and Act
    Manifest actualManifest = new Manifest();

    // Assert
    assertNull(actualManifest.getManifestVersion());
    assertNull(actualManifest.getMainSection().getName());
  }

  /**
   * Test {@link Manifest#Manifest(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code :}.</li>
   *   <li>Then return MainSection Name is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#Manifest(Reader)}
   */
  @Test
  public void testNewManifest_whenStringReaderWithColon_thenReturnMainSectionNameIsNull()
      throws IOException, ManifestException {
    // Arrange and Act
    Manifest actualManifest = new Manifest(new StringReader(": "));

    // Assert
    assertNull(actualManifest.getMainSection().getName());
    assertEquals(Manifest.DEFAULT_MANIFEST_VERSION, actualManifest.getManifestVersion());
  }

  /**
   * Test {@link Manifest#addConfiguredSection(Section)}.
   * <ul>
   *   <li>When {@link Section} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#addConfiguredSection(Section)}
   */
  @Test
  public void testAddConfiguredSection_whenSection_thenThrowBuildException() throws BuildException, ManifestException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();

    // Act and Assert
    assertThrows(BuildException.class, () -> defaultManifest.addConfiguredSection(new Section()));
  }

  /**
   * Test {@link Manifest#addConfiguredAttribute(Attribute)}.
   * <p>
   * Method under test: {@link Manifest#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testAddConfiguredAttribute() throws BuildException, ManifestException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();

    // Act
    defaultManifest.addConfiguredAttribute(new Attribute(" ", "42"));

    // Assert that nothing has changed
    assertEquals(Manifest.DEFAULT_MANIFEST_VERSION, defaultManifest.getManifestVersion());
  }

  /**
   * Test {@link Manifest#addConfiguredAttribute(Attribute)}.
   * <ul>
   *   <li>Given {@code Attributes must have name and value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testAddConfiguredAttribute_givenAttributesMustHaveNameAndValue()
      throws BuildException, ManifestException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();

    Attribute attribute = new Attribute();
    attribute.setName("Attributes must have name and value");

    // Act and Assert
    assertThrows(BuildException.class, () -> defaultManifest.addConfiguredAttribute(attribute));
  }

  /**
   * Test {@link Manifest#addConfiguredAttribute(Attribute)}.
   * <ul>
   *   <li>Then DefaultManifest ManifestVersion is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testAddConfiguredAttribute_thenDefaultManifestManifestVersionIs42()
      throws BuildException, ManifestException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();

    // Act
    defaultManifest.addConfiguredAttribute(new Attribute("manifest-version", "42"));

    // Assert
    assertEquals("42", defaultManifest.getManifestVersion());
  }

  /**
   * Test {@link Manifest#addConfiguredAttribute(Attribute)}.
   * <ul>
   *   <li>When {@link Attribute#Attribute()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testAddConfiguredAttribute_whenAttribute_thenThrowBuildException()
      throws BuildException, ManifestException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();

    // Act and Assert
    assertThrows(BuildException.class, () -> defaultManifest.addConfiguredAttribute(new Attribute()));
  }

  /**
   * Test {@link Manifest#merge(Manifest)} with {@code other}.
   * <ul>
   *   <li>Given {@link Section} (default constructor) Name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#merge(Manifest)}
   */
  @Test
  public void testMergeWithOther_givenSectionNameIsAttribute_name() throws BuildException, ManifestException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();

    Section section = new Section();
    section.setName(Manifest.ATTRIBUTE_NAME);
    Manifest other = Manifest.getDefaultManifest();
    other.addConfiguredSection(section);

    // Act
    defaultManifest.merge(other);

    // Assert
    assertEquals(defaultManifest, other);
  }

  /**
   * Test {@link Manifest#merge(Manifest)} with {@code other}.
   * <ul>
   *   <li>When DefaultManifest.</li>
   *   <li>Then DefaultManifest.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#merge(Manifest)}
   */
  @Test
  public void testMergeWithOther_whenDefaultManifest_thenDefaultManifest() throws BuildException, ManifestException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();
    Manifest other = Manifest.getDefaultManifest();

    // Act
    defaultManifest.merge(other);

    // Assert that nothing has changed
    assertEquals(defaultManifest, other);
  }

  /**
   * Test {@link Manifest#equals(Object)}, and {@link Manifest#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Manifest#equals(Object)}
   *   <li>{@link Manifest#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() throws BuildException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();
    Manifest defaultManifest2 = Manifest.getDefaultManifest();

    // Act and Assert
    assertEquals(defaultManifest, defaultManifest2);
    int expectedHashCodeResult = defaultManifest.hashCode();
    assertEquals(expectedHashCodeResult, defaultManifest2.hashCode());
  }

  /**
   * Test {@link Manifest#equals(Object)}, and {@link Manifest#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Manifest#equals(Object)}
   *   <li>{@link Manifest#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual2() {
    // Arrange
    Manifest manifest = new Manifest();
    Manifest manifest2 = new Manifest();

    // Act and Assert
    assertEquals(manifest, manifest2);
    int expectedHashCodeResult = manifest.hashCode();
    assertEquals(expectedHashCodeResult, manifest2.hashCode());
  }

  /**
   * Test {@link Manifest#equals(Object)}, and {@link Manifest#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Manifest#equals(Object)}
   *   <li>{@link Manifest#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() throws BuildException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();

    // Act and Assert
    assertEquals(defaultManifest, defaultManifest);
    int expectedHashCodeResult = defaultManifest.hashCode();
    assertEquals(expectedHashCodeResult, defaultManifest.hashCode());
  }

  /**
   * Test {@link Manifest#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() throws BuildException {
    // Arrange
    Manifest manifest = new Manifest();

    // Act and Assert
    assertNotEquals(manifest, Manifest.getDefaultManifest());
  }

  /**
   * Test {@link Manifest#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() throws BuildException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();

    // Act and Assert
    assertNotEquals(defaultManifest, new Manifest());
  }

  /**
   * Test {@link Manifest#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual3()
      throws IOException, BuildException, ManifestException {
    // Arrange
    Manifest manifest = new Manifest(new StringReader(""));

    // Act and Assert
    assertNotEquals(manifest, Manifest.getDefaultManifest());
  }

  /**
   * Test {@link Manifest#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() throws BuildException {
    // Arrange, Act and Assert
    assertNotEquals(Manifest.getDefaultManifest(), null);
  }

  /**
   * Test {@link Manifest#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Manifest#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() throws BuildException {
    // Arrange, Act and Assert
    assertNotEquals(Manifest.getDefaultManifest(), "Different type to Manifest");
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Manifest#toString()}
   *   <li>{@link Manifest#getMainSection()}
   *   <li>{@link Manifest#getManifestVersion()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws BuildException {
    // Arrange
    Manifest defaultManifest = Manifest.getDefaultManifest();

    // Act
    String actualToStringResult = defaultManifest.toString();
    Section actualMainSection = defaultManifest.getMainSection();
    String actualManifestVersion = defaultManifest.getManifestVersion();

    // Assert
    assertNull(actualMainSection.getName());
    assertEquals(
        String.join("", "Manifest-Version: 1.0\r\nAnt-Version: Apache Ant 1.10.15\r\nCreated-By: ",
            System.getProperty("java.runtime.version"), " (", System.getProperty("java.vm.vendor"), ")\r\n\r\n"),
        actualToStringResult);
    assertEquals(Manifest.DEFAULT_MANIFEST_VERSION, actualManifestVersion);
  }

  /**
   * Test {@link Manifest#getSection(String)}.
   * <p>
   * Method under test: {@link Manifest#getSection(String)}
   */
  @Test
  public void testGetSection() throws BuildException {
    // Arrange, Act and Assert
    assertNull(Manifest.getDefaultManifest().getSection(Manifest.ATTRIBUTE_NAME));
  }

  /**
   * Test Section {@link Section#addAttributeAndCheck(Attribute)}.
   * <ul>
   *   <li>Given {@code Attributes must have name and value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#addAttributeAndCheck(Attribute)}
   */
  @Test
  public void testSectionAddAttributeAndCheck_givenAttributesMustHaveNameAndValue() throws ManifestException {
    // Arrange
    Section section = new Section();

    Attribute attribute = new Attribute();
    attribute.setName("Attributes must have name and value");

    // Act and Assert
    assertThrows(BuildException.class, () -> section.addAttributeAndCheck(attribute));
  }

  /**
   * Test Section {@link Section#addAttributeAndCheck(Attribute)}.
   * <ul>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#addAttributeAndCheck(Attribute)}
   */
  @Test
  public void testSectionAddAttributeAndCheck_thenReturn42() throws ManifestException {
    // Arrange
    Section section = new Section();

    // Act and Assert
    assertEquals("42", section.addAttributeAndCheck(new Attribute(Manifest.ATTRIBUTE_NAME, "42")));
  }

  /**
   * Test Section {@link Section#addAttributeAndCheck(Attribute)}.
   * <ul>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#addAttributeAndCheck(Attribute)}
   */
  @Test
  public void testSectionAddAttributeAndCheck_thenReturnNull() throws ManifestException {
    // Arrange
    Section section = new Section();

    // Act and Assert
    assertNull(section.addAttributeAndCheck(new Attribute(" ", "42")));
  }

  /**
   * Test Section {@link Section#addAttributeAndCheck(Attribute)}.
   * <ul>
   *   <li>When {@link Attribute#Attribute()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#addAttributeAndCheck(Attribute)}
   */
  @Test
  public void testSectionAddAttributeAndCheck_whenAttribute_thenThrowBuildException() throws ManifestException {
    // Arrange
    Section section = new Section();

    // Act and Assert
    assertThrows(BuildException.class, () -> section.addAttributeAndCheck(new Attribute()));
  }

  /**
   * Test Section {@link Section#addConfiguredAttribute(Attribute)}.
   * <p>
   * Method under test: {@link Section#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testSectionAddConfiguredAttribute() throws ManifestException {
    // Arrange
    Section section = new Section();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> section.addConfiguredAttribute(new Attribute(Manifest.ATTRIBUTE_NAME, "42")));
  }

  /**
   * Test Section {@link Section#addConfiguredAttribute(Attribute)}.
   * <ul>
   *   <li>Given {@code Attributes must have name and value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testSectionAddConfiguredAttribute_givenAttributesMustHaveNameAndValue() throws ManifestException {
    // Arrange
    Section section = new Section();

    Attribute attribute = new Attribute();
    attribute.setName("Attributes must have name and value");

    // Act and Assert
    assertThrows(BuildException.class, () -> section.addConfiguredAttribute(attribute));
  }

  /**
   * Test Section {@link Section#addConfiguredAttribute(Attribute)}.
   * <ul>
   *   <li>When {@link Attribute#Attribute()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#addConfiguredAttribute(Attribute)}
   */
  @Test
  public void testSectionAddConfiguredAttribute_whenAttribute_thenThrowBuildException() throws ManifestException {
    // Arrange
    Section section = new Section();

    // Act and Assert
    assertThrows(BuildException.class, () -> section.addConfiguredAttribute(new Attribute()));
  }

  /**
   * Test Section {@link Section#clone()}.
   * <p>
   * Method under test: {@link Section#clone()}
   */
  @Test
  public void testSectionClone() {
    // Arrange and Act
    Object actualCloneResult = (new Section()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof Section);
    assertNull(((Section) actualCloneResult).getName());
  }

  /**
   * Test Section {@link Section#equals(Object)}, and {@link Section#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Section#equals(Object)}
   *   <li>{@link Section#hashCode()}
   * </ul>
   */
  @Test
  public void testSectionEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    Section section = new Section();
    Section section2 = new Section();

    // Act and Assert
    assertEquals(section, section2);
    int expectedHashCodeResult = section.hashCode();
    assertEquals(expectedHashCodeResult, section2.hashCode());
  }

  /**
   * Test Section {@link Section#equals(Object)}, and {@link Section#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Section#equals(Object)}
   *   <li>{@link Section#hashCode()}
   * </ul>
   */
  @Test
  public void testSectionEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    Section section = new Section();

    // Act and Assert
    assertEquals(section, section);
    int expectedHashCodeResult = section.hashCode();
    assertEquals(expectedHashCodeResult, section.hashCode());
  }

  /**
   * Test Section {@link Section#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#equals(Object)}
   */
  @Test
  public void testSectionEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new Section(), null);
  }

  /**
   * Test Section {@link Section#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#equals(Object)}
   */
  @Test
  public void testSectionEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new Section(), null);
  }

  /**
   * Test Section {@link Section#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#equals(Object)}
   */
  @Test
  public void testSectionEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new Section(), "Different type to Section");
  }

  /**
   * Test Section {@link Section#getAttribute(String)}.
   * <p>
   * Method under test: {@link Section#getAttribute(String)}
   */
  @Test
  public void testSectionGetAttribute() {
    // Arrange, Act and Assert
    assertNull((new Section()).getAttribute("Attribute Name"));
  }

  /**
   * Test Section {@link Section#getAttributeValue(String)}.
   * <ul>
   *   <li>When {@code Attribute Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#getAttributeValue(String)}
   */
  @Test
  public void testSectionGetAttributeValue_whenAttributeName_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Section()).getAttributeValue("Attribute Name"));
  }

  /**
   * Test Section getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Section}
   *   <li>{@link Section#setName(String)}
   *   <li>{@link Section#getName()}
   * </ul>
   */
  @Test
  public void testSectionGettersAndSetters() {
    // Arrange and Act
    Section actualSection = new Section();
    actualSection.setName(Manifest.ATTRIBUTE_NAME);

    // Assert
    assertEquals(Manifest.ATTRIBUTE_NAME, actualSection.getName());
  }

  /**
   * Test Section {@link Section#merge(Section)} with {@code section}.
   * <p>
   * Method under test: {@link Section#merge(Section)}
   */
  @Test
  public void testSectionMergeWithSection() throws ManifestException {
    // Arrange
    Section section = new Section();
    section.setName("Unable to merge sections with different names");

    Section section2 = new Section();
    section2.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertThrows(ManifestException.class, () -> section.merge(section2));
  }

  /**
   * Test Section {@link Section#merge(Section, boolean)} with {@code section}, {@code mergeClassPaths}.
   * <p>
   * Method under test: {@link Section#merge(Section, boolean)}
   */
  @Test
  public void testSectionMergeWithSectionMergeClassPaths() throws ManifestException {
    // Arrange
    Section section = new Section();
    section.setName("Unable to merge sections with different names");

    Section section2 = new Section();
    section2.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertThrows(ManifestException.class, () -> section.merge(section2, true));
  }

  /**
   * Test Section {@link Section#merge(Section, boolean)} with {@code section}, {@code mergeClassPaths}.
   * <ul>
   *   <li>Then throw {@link ManifestException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#merge(Section, boolean)}
   */
  @Test
  public void testSectionMergeWithSectionMergeClassPaths_thenThrowManifestException() throws ManifestException {
    // Arrange
    Section section = new Section();

    Section section2 = new Section();
    section2.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertThrows(ManifestException.class, () -> section.merge(section2, true));
  }

  /**
   * Test Section {@link Section#merge(Section)} with {@code section}.
   * <ul>
   *   <li>Given {@link Section} (default constructor).</li>
   *   <li>Then throw {@link ManifestException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#merge(Section)}
   */
  @Test
  public void testSectionMergeWithSection_givenSection_thenThrowManifestException() throws ManifestException {
    // Arrange
    Section section = new Section();

    Section section2 = new Section();
    section2.setName(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertThrows(ManifestException.class, () -> section.merge(section2));
  }

  /**
   * Test Section {@link Section#read(BufferedReader)}.
   * <ul>
   *   <li>Given {@link Section} (default constructor) addConfiguredAttribute {@link Attribute#Attribute(String)} with line is {@code :}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#read(BufferedReader)}
   */
  @Test
  public void testSectionRead_givenSectionAddConfiguredAttributeAttributeWithLineIsColon()
      throws IOException, ManifestException {
    // Arrange
    Section section = new Section();
    section.addConfiguredAttribute(new Attribute(": "));

    // Act and Assert
    assertThrows(ManifestException.class, () -> section.read(new BufferedReader(new StringReader(": "), 1)));
  }

  /**
   * Test Section {@link Section#read(BufferedReader)}.
   * <ul>
   *   <li>Given {@link Section} (default constructor).</li>
   *   <li>When {@link StringReader#StringReader(String)} with {@code :}.</li>
   *   <li>Then {@link Section} (default constructor) Name is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#read(BufferedReader)}
   */
  @Test
  public void testSectionRead_givenSection_whenStringReaderWithColon_thenSectionNameIsNull()
      throws IOException, ManifestException {
    // Arrange
    Section section = new Section();

    // Act
    String actualReadResult = section.read(new BufferedReader(new StringReader(": "), 1));

    // Assert
    assertNull(section.getName());
    assertNull(actualReadResult);
  }

  /**
   * Test Section {@link Section#read(BufferedReader)}.
   * <ul>
   *   <li>Then {@link Section} (default constructor) Name is {@code Can't start an attribute with a continuation line f f}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#read(BufferedReader)}
   */
  @Test
  public void testSectionRead_thenSectionNameIsCanTStartAnAttributeWithAContinuationLineFF()
      throws IOException, ManifestException {
    // Arrange
    Section section = new Section();
    section.setName("Can't start an attribute with a continuation line ");

    // Act
    String actualReadResult = section.read(new BufferedReader(new CharArrayReader(" f f".toCharArray()), 1));

    // Assert
    assertEquals("Can't start an attribute with a continuation line f f", section.getName());
    assertNull(actualReadResult);
  }

  /**
   * Test Section {@link Section#read(BufferedReader)}.
   * <ul>
   *   <li>When {@link CharArrayReader#CharArrayReader(char[])} with {@code f f} toCharArray.</li>
   *   <li>Then throw {@link ManifestException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Section#read(BufferedReader)}
   */
  @Test
  public void testSectionRead_whenCharArrayReaderWithFFToCharArray_thenThrowManifestException()
      throws IOException, ManifestException {
    // Arrange
    Section section = new Section();

    // Act and Assert
    assertThrows(ManifestException.class,
        () -> section.read(new BufferedReader(new CharArrayReader(" f f".toCharArray()), 1)));
  }
}
