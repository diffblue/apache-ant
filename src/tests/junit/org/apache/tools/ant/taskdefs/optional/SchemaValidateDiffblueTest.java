package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.optional.SchemaValidate.SchemaLocation;
import org.apache.tools.ant.types.XMLCatalog;
import org.junit.Test;

public class SchemaValidateDiffblueTest {
  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}, and {@link SchemaLocation#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SchemaLocation#equals(Object)}
   *   <li>{@link SchemaLocation#hashCode()}
   * </ul>
   */
  @Test
  public void testSchemaLocationEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    SchemaLocation schemaLocation2 = new SchemaLocation();

    // Act and Assert
    assertEquals(schemaLocation, schemaLocation2);
    int expectedHashCodeResult = schemaLocation.hashCode();
    assertEquals(expectedHashCodeResult, schemaLocation2.hashCode());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}, and {@link SchemaLocation#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SchemaLocation#equals(Object)}
   *   <li>{@link SchemaLocation#hashCode()}
   * </ul>
   */
  @Test
  public void testSchemaLocationEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual2() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setNamespace("Namespace");

    SchemaLocation schemaLocation2 = new SchemaLocation();
    schemaLocation2.setNamespace("Namespace");

    // Act and Assert
    assertEquals(schemaLocation, schemaLocation2);
    int expectedHashCodeResult = schemaLocation.hashCode();
    assertEquals(expectedHashCodeResult, schemaLocation2.hashCode());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}, and {@link SchemaLocation#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SchemaLocation#equals(Object)}
   *   <li>{@link SchemaLocation#hashCode()}
   * </ul>
   */
  @Test
  public void testSchemaLocationEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual3() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    SchemaLocation schemaLocation2 = new SchemaLocation();
    schemaLocation2.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(schemaLocation, schemaLocation2);
    int expectedHashCodeResult = schemaLocation.hashCode();
    assertEquals(expectedHashCodeResult, schemaLocation2.hashCode());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}, and {@link SchemaLocation#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SchemaLocation#equals(Object)}
   *   <li>{@link SchemaLocation#hashCode()}
   * </ul>
   */
  @Test
  public void testSchemaLocationEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual4() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setUrl("https://example.org/example");

    SchemaLocation schemaLocation2 = new SchemaLocation();
    schemaLocation2.setUrl("https://example.org/example");

    // Act and Assert
    assertEquals(schemaLocation, schemaLocation2);
    int expectedHashCodeResult = schemaLocation.hashCode();
    assertEquals(expectedHashCodeResult, schemaLocation2.hashCode());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}, and {@link SchemaLocation#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link SchemaLocation#equals(Object)}
   *   <li>{@link SchemaLocation#hashCode()}
   * </ul>
   */
  @Test
  public void testSchemaLocationEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();

    // Act and Assert
    assertEquals(schemaLocation, schemaLocation);
    int expectedHashCodeResult = schemaLocation.hashCode();
    assertEquals(expectedHashCodeResult, schemaLocation.hashCode());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#equals(Object)}
   */
  @Test
  public void testSchemaLocationEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new SchemaLocation(), 1);
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#equals(Object)}
   */
  @Test
  public void testSchemaLocationEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setNamespace("Namespace");

    // Act and Assert
    assertNotEquals(schemaLocation, new SchemaLocation());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#equals(Object)}
   */
  @Test
  public void testSchemaLocationEquals_whenOtherIsDifferent_thenReturnNotEqual3() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNotEquals(schemaLocation, new SchemaLocation());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#equals(Object)}
   */
  @Test
  public void testSchemaLocationEquals_whenOtherIsDifferent_thenReturnNotEqual4() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setUrl("https://example.org/example");

    // Act and Assert
    assertNotEquals(schemaLocation, new SchemaLocation());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#equals(Object)}
   */
  @Test
  public void testSchemaLocationEquals_whenOtherIsDifferent_thenReturnNotEqual5() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();

    SchemaLocation schemaLocation2 = new SchemaLocation();
    schemaLocation2.setNamespace("Namespace");

    // Act and Assert
    assertNotEquals(schemaLocation, schemaLocation2);
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#equals(Object)}
   */
  @Test
  public void testSchemaLocationEquals_whenOtherIsDifferent_thenReturnNotEqual6() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();

    SchemaLocation schemaLocation2 = new SchemaLocation();
    schemaLocation2.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNotEquals(schemaLocation, schemaLocation2);
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#equals(Object)}
   */
  @Test
  public void testSchemaLocationEquals_whenOtherIsDifferent_thenReturnNotEqual7() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();

    SchemaLocation schemaLocation2 = new SchemaLocation();
    schemaLocation2.setUrl("https://example.org/example");

    // Act and Assert
    assertNotEquals(schemaLocation, schemaLocation2);
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#equals(Object)}
   */
  @Test
  public void testSchemaLocationEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new SchemaLocation(), null);
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#equals(Object)}
   */
  @Test
  public void testSchemaLocationEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new SchemaLocation(), "Different type to SchemaLocation");
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#getSchemaLocationURL()}.
   * <p>
   * Method under test: {@link SchemaLocation#getSchemaLocationURL()}
   */
  @Test
  public void testSchemaLocationGetSchemaLocationURL() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    schemaLocation.setUrl("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaLocation.getSchemaLocationURL());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#getSchemaLocationURL()}.
   * <p>
   * Method under test: {@link SchemaLocation#getSchemaLocationURL()}
   */
  @Test
  public void testSchemaLocationGetSchemaLocationURL2() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "foo", "foo", "foo").toFile());
    schemaLocation.setUrl(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaLocation.getSchemaLocationURL());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#getSchemaLocationURL()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#getSchemaLocationURL()}
   */
  @Test
  public void testSchemaLocationGetSchemaLocationURL_givenSchemaLocation() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SchemaLocation()).getSchemaLocationURL());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#getSchemaLocationURL()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor) Url is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#getSchemaLocationURL()}
   */
  @Test
  public void testSchemaLocationGetSchemaLocationURL_givenSchemaLocationUrlIsEmptyString() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setFile(null);
    schemaLocation.setUrl("");

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaLocation.getSchemaLocationURL());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#getSchemaLocationURL()}.
   * <ul>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#getSchemaLocationURL()}
   */
  @Test
  public void testSchemaLocationGetSchemaLocationURL_thenReturnFoo() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setFile(null);
    schemaLocation.setUrl("foo");

    // Act and Assert
    assertEquals("foo", schemaLocation.getSchemaLocationURL());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#getURIandLocation()}.
   * <p>
   * Method under test: {@link SchemaLocation#getURIandLocation()}
   */
  @Test
  public void testSchemaLocationGetURIandLocation() throws BuildException {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    schemaLocation.setUrl("foo");
    schemaLocation.setNamespace("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaLocation.getURIandLocation());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#getURIandLocation()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor) File is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#getURIandLocation()}
   */
  @Test
  public void testSchemaLocationGetURIandLocation_givenSchemaLocationFileIsNull() throws BuildException {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setFile(null);
    schemaLocation.setUrl(null);
    schemaLocation.setNamespace("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaLocation.getURIandLocation());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#getURIandLocation()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor) Namespace is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#getURIandLocation()}
   */
  @Test
  public void testSchemaLocationGetURIandLocation_givenSchemaLocationNamespaceIsEmptyString() throws BuildException {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setFile(null);
    schemaLocation.setUrl(null);
    schemaLocation.setNamespace("");

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaLocation.getURIandLocation());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#getURIandLocation()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor) Url is {@code foo}.</li>
   *   <li>Then return {@code foo foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#getURIandLocation()}
   */
  @Test
  public void testSchemaLocationGetURIandLocation_givenSchemaLocationUrlIsFoo_thenReturnFooFoo() throws BuildException {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setFile(null);
    schemaLocation.setUrl("foo");
    schemaLocation.setNamespace("foo");

    // Act and Assert
    assertEquals("foo foo", schemaLocation.getURIandLocation());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#getURIandLocation()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#getURIandLocation()}
   */
  @Test
  public void testSchemaLocationGetURIandLocation_givenSchemaLocation_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SchemaLocation()).getURIandLocation());
  }

  /**
   * Test SchemaLocation getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link SchemaLocation}
   *   <li>{@link SchemaLocation#setFile(File)}
   *   <li>{@link SchemaLocation#setNamespace(String)}
   *   <li>{@link SchemaLocation#setUrl(String)}
   *   <li>{@link SchemaLocation#getFile()}
   *   <li>{@link SchemaLocation#getNamespace()}
   *   <li>{@link SchemaLocation#getUrl()}
   * </ul>
   */
  @Test
  public void testSchemaLocationGettersAndSetters() {
    // Arrange and Act
    SchemaLocation actualSchemaLocation = new SchemaLocation();
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    actualSchemaLocation.setFile(file);
    actualSchemaLocation.setNamespace("Namespace");
    actualSchemaLocation.setUrl("https://example.org/example");
    File actualFile = actualSchemaLocation.getFile();
    String actualNamespace = actualSchemaLocation.getNamespace();

    // Assert
    assertEquals("Namespace", actualNamespace);
    assertEquals("https://example.org/example", actualSchemaLocation.getUrl());
    assertSame(file, actualFile);
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#toString()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor) Namespace is {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#toString()}
   */
  @Test
  public void testSchemaLocationToString_givenSchemaLocationNamespaceIsFoo_thenReturnFoo() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setNamespace("foo");
    schemaLocation.setUrl(null);
    schemaLocation.setFile(null);

    // Act and Assert
    assertEquals("foo", schemaLocation.toString());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#toString()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor) Url is {@code foo}.</li>
   *   <li>Then return {@code (anonymous) foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#toString()}
   */
  @Test
  public void testSchemaLocationToString_givenSchemaLocationUrlIsFoo_thenReturnAnonymousFoo() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setNamespace(null);
    schemaLocation.setUrl("foo");
    schemaLocation.setFile(null);

    // Act and Assert
    assertEquals("(anonymous) foo", schemaLocation.toString());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#toString()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor).</li>
   *   <li>Then return {@code (anonymous)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#toString()}
   */
  @Test
  public void testSchemaLocationToString_givenSchemaLocation_thenReturnAnonymous() {
    // Arrange, Act and Assert
    assertEquals("(anonymous)", (new SchemaLocation()).toString());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#validateNamespace()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor) Namespace is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#validateNamespace()}
   */
  @Test
  public void testSchemaLocationValidateNamespace_givenSchemaLocationNamespaceIsEmptyString() {
    // Arrange
    SchemaLocation schemaLocation = new SchemaLocation();
    schemaLocation.setNamespace("");

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaLocation.validateNamespace());
  }

  /**
   * Test SchemaLocation {@link SchemaLocation#validateNamespace()}.
   * <ul>
   *   <li>Given {@link SchemaLocation} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaLocation#validateNamespace()}
   */
  @Test
  public void testSchemaLocationValidateNamespace_givenSchemaLocation_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new SchemaLocation()).validateNamespace());
  }

  /**
   * Test {@link SchemaValidate#setNoNamespaceURL(String)}.
   * <ul>
   *   <li>Given {@link SchemaValidate} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaValidate#setNoNamespaceURL(String)}
   */
  @Test
  public void testSetNoNamespaceURL_givenSchemaValidate() {
    // Arrange
    SchemaValidate schemaValidate = new SchemaValidate();

    // Act
    schemaValidate.setNoNamespaceURL("https://example.org/example");

    // Assert
    assertEquals("https://example.org/example", schemaValidate.getNoNamespaceSchemaURL());
  }

  /**
   * Test {@link SchemaValidate#setNoNamespaceURL(String)}.
   * <ul>
   *   <li>Given {@link SchemaValidate} (default constructor) NoNamespaceURL is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaValidate#setNoNamespaceURL(String)}
   */
  @Test
  public void testSetNoNamespaceURL_givenSchemaValidateNoNamespaceURLIsNull() {
    // Arrange
    SchemaValidate schemaValidate = new SchemaValidate();
    schemaValidate.setNoNamespaceURL(null);

    // Act
    schemaValidate.setNoNamespaceURL("https://example.org/example");

    // Assert
    assertEquals("https://example.org/example", schemaValidate.getNoNamespaceSchemaURL());
  }

  /**
   * Test {@link SchemaValidate#initValidator()}.
   * <p>
   * Method under test: {@link SchemaValidate#initValidator()}
   */
  @Test
  public void testInitValidator() {
    // Arrange
    SchemaValidate schemaValidate = new SchemaValidate();
    schemaValidate.setNoNamespaceFile(Paths.get(System.getProperty("java.io.tmpdir"), "Using SAX2 reader ").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaValidate.initValidator());
  }

  /**
   * Test {@link SchemaValidate#initValidator()}.
   * <ul>
   *   <li>Given {@link SchemaValidate} (default constructor) NoNamespaceURL is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaValidate#initValidator()}
   */
  @Test
  public void testInitValidator_givenSchemaValidateNoNamespaceURLIsEmptyString() {
    // Arrange
    SchemaValidate schemaValidate = new SchemaValidate();
    schemaValidate.setNoNamespaceURL("");

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaValidate.initValidator());
  }

  /**
   * Test {@link SchemaValidate#initValidator()}.
   * <ul>
   *   <li>Given {@link SchemaValidate} (default constructor) NoNamespaceURL is {@code https://example.org/example}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaValidate#initValidator()}
   */
  @Test
  public void testInitValidator_givenSchemaValidateNoNamespaceURLIsHttpsExampleOrgExample() {
    // Arrange
    SchemaValidate schemaValidate = new SchemaValidate();
    schemaValidate.setNoNamespaceURL("https://example.org/example");
    schemaValidate.setNoNamespaceFile(Paths.get(System.getProperty("java.io.tmpdir"), "Using SAX2 reader ").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaValidate.initValidator());
  }

  /**
   * Test {@link SchemaValidate#initValidator()}.
   * <ul>
   *   <li>Given {@link SchemaValidate} (default constructor) NoNamespaceURL is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaValidate#initValidator()}
   */
  @Test
  public void testInitValidator_givenSchemaValidateNoNamespaceURLIsNull() {
    // Arrange
    SchemaValidate schemaValidate = new SchemaValidate();
    schemaValidate.setNoNamespaceURL(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> schemaValidate.initValidator());
  }

  /**
   * Test {@link SchemaValidate#getNoNamespaceSchemaURL()}.
   * <ul>
   *   <li>Given {@link SchemaValidate} (default constructor) NoNamespaceURL is {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaValidate#getNoNamespaceSchemaURL()}
   */
  @Test
  public void testGetNoNamespaceSchemaURL_givenSchemaValidateNoNamespaceURLIsFoo_thenReturnFoo() {
    // Arrange
    SchemaValidate schemaValidate = new SchemaValidate();
    schemaValidate.setNoNamespaceURL("foo");

    // Act and Assert
    assertEquals("foo", schemaValidate.getNoNamespaceSchemaURL());
  }

  /**
   * Test {@link SchemaValidate#getNoNamespaceSchemaURL()}.
   * <ul>
   *   <li>Given {@link SchemaValidate} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SchemaValidate#getNoNamespaceSchemaURL()}
   */
  @Test
  public void testGetNoNamespaceSchemaURL_givenSchemaValidate_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new SchemaValidate()).getNoNamespaceSchemaURL());
  }

  /**
   * Test new {@link SchemaValidate} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SchemaValidate}
   */
  @Test
  public void testNewSchemaValidate() {
    // Arrange and Act
    SchemaValidate actualSchemaValidate = new SchemaValidate();

    // Assert
    assertTrue(actualSchemaValidate.getEntityResolver() instanceof XMLCatalog);
    assertNull(actualSchemaValidate.file);
    assertNull(actualSchemaValidate.getDescription());
    assertNull(actualSchemaValidate.getTaskName());
    assertNull(actualSchemaValidate.getTaskType());
    assertNull(actualSchemaValidate.getNoNamespaceSchemaURL());
    assertNull(actualSchemaValidate.readerClassName);
    assertNull(actualSchemaValidate.getProject());
    assertNull(actualSchemaValidate.getOwningTarget());
    assertNull(actualSchemaValidate.classpath);
    assertNull(actualSchemaValidate.getXmlReader());
    assertFalse(actualSchemaValidate.isSax1Parser());
    assertFalse(actualSchemaValidate.lenient);
    assertTrue(actualSchemaValidate.filesets.isEmpty());
    assertTrue(actualSchemaValidate.failOnError);
    assertTrue(actualSchemaValidate.warn);
  }
}
