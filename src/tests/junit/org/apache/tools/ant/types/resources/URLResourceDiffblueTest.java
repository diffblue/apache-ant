package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class URLResourceDiffblueTest {
  /**
   * Test {@link URLResource#URLResource(URLProvider)}.
   * <ul>
   *   <li>Then return {@link URLResource#URLResource(String)} with u is {@code https://example.org/example}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#URLResource(URLProvider)}
   */
  @Test
  public void testNewURLResource_thenReturnURLResourceWithUIsHttpsExampleOrgExample() {
    // Arrange
    URLResource u = new URLResource("https://example.org/example");

    // Act and Assert
    assertEquals(u, new URLResource(u));
  }

  /**
   * Test {@link URLResource#URLResource(URLProvider)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#URLResource(URLProvider)}
   */
  @Test
  public void testNewURLResource_thenThrowBuildException() throws MalformedURLException {
    // Arrange
    URLResource u = new URLResource();
    u.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> new URLResource(u));
  }

  /**
   * Test {@link URLResource#URLResource(String)}.
   * <ul>
   *   <li>When {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#URLResource(String)}
   */
  @Test
  public void testNewURLResource_whenFoo_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> new URLResource("foo"));
  }

  /**
   * Test {@link URLResource#URLResource(URLProvider)}.
   * <ul>
   *   <li>When {@link URLResource#URLResource()}.</li>
   *   <li>Then return {@link URLResource#URLResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#URLResource(URLProvider)}
   */
  @Test
  public void testNewURLResource_whenURLResource_thenReturnURLResource() {
    // Arrange
    URLResource u = new URLResource();

    // Act and Assert
    assertEquals(u, new URLResource(u));
  }

  /**
   * Test {@link URLResource#setBaseURL(URL)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#setBaseURL(URL)}
   */
  @Test
  public void testSetBaseURL_thenThrowBuildException() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL()));
  }

  /**
   * Test {@link URLResource#setRelativePath(String)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#setRelativePath(String)}
   */
  @Test
  public void testSetRelativePath_thenThrowBuildException() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.setRelativePath("https://example.org/example"));
  }

  /**
   * Test {@link URLResource#getURL()}.
   * <p>
   * Method under test: {@link URLResource#getURL()}
   */
  @Test
  public void testGetURL() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setURL(null);
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    urlResource.setRelativePath("foo");

    // Act
    URL actualURL = urlResource.getURL();

    // Assert
    String expectedToStringResult = String.join("", "file:",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", "foo").toString());
    assertEquals(expectedToStringResult, actualURL.toString());
  }

  /**
   * Test {@link URLResource#getURL()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()} RelativePath is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getURL()}
   */
  @Test
  public void testGetURL_givenURLResourceRelativePathIsNull_thenThrowBuildException() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setURL(null);
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    urlResource.setRelativePath(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.getURL());
  }

  /**
   * Test {@link URLResource#getURL()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getURL()}
   */
  @Test
  public void testGetURL_givenURLResource_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new URLResource()).getURL());
  }

  /**
   * Test {@link URLResource#getURL()}.
   * <ul>
   *   <li>Then return toString is {@code https://example.org/example}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getURL()}
   */
  @Test
  public void testGetURL_thenReturnToStringIsHttpsExampleOrgExample() {
    // Arrange, Act and Assert
    assertEquals("https://example.org/example", (new URLResource("https://example.org/example")).getURL().toString());
  }

  /**
   * Test {@link URLResource#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()}.</li>
   *   <li>Then {@link URLResource#URLResource()} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenURLResource_thenURLResourceReference() {
    // Arrange
    URLResource urlResource = new URLResource();
    Reference r = new Reference("42");

    // Act
    urlResource.setRefid(r);

    // Assert
    assertTrue(urlResource.isReference());
    assertSame(r, urlResource.getRefid());
  }

  /**
   * Test {@link URLResource#getName()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()} RelativePath is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getName()}
   */
  @Test
  public void testGetName_givenURLResourceRelativePathIsNull_thenThrowBuildException() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setURL(null);
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    urlResource.setRelativePath(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.getName());
  }

  /**
   * Test {@link URLResource#getName()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource(String)} with u is {@code https://example.org/example}.</li>
   *   <li>Then return {@code example}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getName()}
   */
  @Test
  public void testGetName_givenURLResourceWithUIsHttpsExampleOrgExample_thenReturnExample() {
    // Arrange, Act and Assert
    assertEquals("example", (new URLResource("https://example.org/example")).getName());
  }

  /**
   * Test {@link URLResource#toString()}.
   * <p>
   * Method under test: {@link URLResource#toString()}
   */
  @Test
  public void testToString() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setURL(null);
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    urlResource.setRelativePath("foo");

    // Act
    String actualToStringResult = urlResource.toString();

    // Assert
    assertEquals(
        String.join("", "file:", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", "foo").toString()),
        actualToStringResult);
  }

  /**
   * Test {@link URLResource#toString()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()} RelativePath is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#toString()}
   */
  @Test
  public void testToString_givenURLResourceRelativePathIsNull_thenThrowBuildException() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setURL(null);
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    urlResource.setRelativePath(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.toString());
  }

  /**
   * Test {@link URLResource#toString()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#toString()}
   */
  @Test
  public void testToString_givenURLResource_thenReturnNull() {
    // Arrange, Act and Assert
    assertEquals("null", (new URLResource()).toString());
  }

  /**
   * Test {@link URLResource#toString()}.
   * <ul>
   *   <li>Then return {@code https://example.org/example}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#toString()}
   */
  @Test
  public void testToString_thenReturnHttpsExampleOrgExample() {
    // Arrange, Act and Assert
    assertEquals("https://example.org/example", (new URLResource("https://example.org/example")).toString());
  }

  /**
   * Test {@link URLResource#isExists()}.
   * <p>
   * Method under test: {@link URLResource#isExists()}
   */
  @Test
  public void testIsExists() throws IOException {
    // Arrange
    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    urlResource.connect(3);

    // Act and Assert
    assertTrue(urlResource.isExists());
  }

  /**
   * Test {@link URLResource#isExists()}.
   * <p>
   * Method under test: {@link URLResource#isExists()}
   */
  @Test
  public void testIsExists2() {
    // Arrange
    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile());
    urlResource.setProject(new Project());

    // Act and Assert
    assertFalse(urlResource.isExists());
  }

  /**
   * Test {@link URLResource#isExists()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#isExists()}
   */
  @Test
  public void testIsExists_givenJavaLangObject_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("\".", typeClass);
    project.addBuildListener(new AntClassLoader());

    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile());
    urlResource.setProject(project);

    // Act and Assert
    assertFalse(urlResource.isExists());
  }

  /**
   * Test {@link URLResource#isExists()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#isExists()}
   */
  @Test
  public void testIsExists_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile());
    urlResource.setProject(project);

    // Act and Assert
    assertFalse(urlResource.isExists());
  }

  /**
   * Test {@link URLResource#isExists()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource(File)} with f is Property is {@code java.io.tmpdir} is {@code ftp} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#isExists()}
   */
  @Test
  public void testIsExists_givenURLResourceWithFIsPropertyIsJavaIoTmpdirIsFtpToFile() {
    // Arrange, Act and Assert
    assertFalse((new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile())).isExists());
  }

  /**
   * Test {@link URLResource#isExists()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#isExists()}
   */
  @Test
  public void testIsExists_givenURLResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange, Act and Assert
    assertTrue((new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).isExists());
  }

  /**
   * Test {@link URLResource#isExists()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#isExists()}
   */
  @Test
  public void testIsExists_givenURLResource_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new URLResource()).isExists());
  }

  /**
   * Test {@link URLResource#isExists()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#isExists()}
   */
  @Test
  public void testIsExists_thenThrowBuildException() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.isExists());
  }

  /**
   * Test {@link URLResource#getLastModified()}.
   * <p>
   * Method under test: {@link URLResource#getLastModified()}
   */
  @Test
  public void testGetLastModified() {
    // Arrange
    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile());
    urlResource.setProject(new Project());

    // Act and Assert
    assertEquals(0L, urlResource.getLastModified());
  }

  /**
   * Test {@link URLResource#getLastModified()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenJavaLangObject_thenReturnZero() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("\".", typeClass);
    project.addBuildListener(new AntClassLoader());

    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile());
    urlResource.setProject(project);

    // Act and Assert
    assertEquals(0L, urlResource.getLastModified());
  }

  /**
   * Test {@link URLResource#getLastModified()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenProjectAddBuildListenerAntClassLoader_thenReturnZero() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile());
    urlResource.setProject(project);

    // Act and Assert
    assertEquals(0L, urlResource.getLastModified());
  }

  /**
   * Test {@link URLResource#getLastModified()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource(File)} with f is Property is {@code java.io.tmpdir} is {@code ftp} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenURLResourceWithFIsPropertyIsJavaIoTmpdirIsFtpToFile() {
    // Arrange, Act and Assert
    assertEquals(0L,
        (new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile())).getLastModified());
  }

  /**
   * Test {@link URLResource#getLastModified()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenURLResource_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0L, (new URLResource()).getLastModified());
  }

  /**
   * Test {@link URLResource#getLastModified()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getLastModified()}
   */
  @Test
  public void testGetLastModified_thenThrowBuildException() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.getLastModified());
  }

  /**
   * Test {@link URLResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()} RelativePath is {@code foo}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenURLResourceRelativePathIsFoo_thenReturnFalse() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setURL(null);
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    urlResource.setRelativePath("foo");

    // Act and Assert
    assertFalse(urlResource.isDirectory());
  }

  /**
   * Test {@link URLResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()} RelativePath is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenURLResourceRelativePathIsNull_thenThrowBuildException()
      throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setURL(null);
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    urlResource.setRelativePath(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.isDirectory());
  }

  /**
   * Test {@link URLResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource(String)} with u is {@code https://example.org/example}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenURLResourceWithUIsHttpsExampleOrgExample_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new URLResource("https://example.org/example")).isDirectory());
  }

  /**
   * Test {@link URLResource#isDirectory()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).isDirectory());
  }

  /**
   * Test {@link URLResource#getSize()}.
   * <p>
   * Method under test: {@link URLResource#getSize()}
   */
  @Test
  public void testGetSize() throws IOException {
    // Arrange
    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    urlResource.connect(3);

    // Act and Assert
    assertEquals(64L, urlResource.getSize());
  }

  /**
   * Test {@link URLResource#getSize()}.
   * <p>
   * Method under test: {@link URLResource#getSize()}
   */
  @Test
  public void testGetSize2() {
    // Arrange
    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile());
    urlResource.setProject(new Project());

    // Act and Assert
    assertEquals(0L, urlResource.getSize());
  }

  /**
   * Test {@link URLResource#getSize()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getSize()}
   */
  @Test
  public void testGetSize_givenJavaLangObject_thenReturnZero() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("\".", typeClass);
    project.addBuildListener(new AntClassLoader());

    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile());
    urlResource.setProject(project);

    // Act and Assert
    assertEquals(0L, urlResource.getSize());
  }

  /**
   * Test {@link URLResource#getSize()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getSize()}
   */
  @Test
  public void testGetSize_givenProjectAddBuildListenerAntClassLoader_thenReturnZero() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile());
    urlResource.setProject(project);

    // Act and Assert
    assertEquals(0L, urlResource.getSize());
  }

  /**
   * Test {@link URLResource#getSize()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource(File)} with f is Property is {@code java.io.tmpdir} is {@code ftp} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getSize()}
   */
  @Test
  public void testGetSize_givenURLResourceWithFIsPropertyIsJavaIoTmpdirIsFtpToFile() {
    // Arrange, Act and Assert
    assertEquals(0L, (new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "ftp").toFile())).getSize());
  }

  /**
   * Test {@link URLResource#getSize()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getSize()}
   */
  @Test
  public void testGetSize_givenURLResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange, Act and Assert
    assertEquals(64L,
        (new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).getSize());
  }

  /**
   * Test {@link URLResource#getSize()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getSize()}
   */
  @Test
  public void testGetSize_givenURLResource_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0L, (new URLResource()).getSize());
  }

  /**
   * Test {@link URLResource#getSize()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getSize()}
   */
  @Test
  public void testGetSize_thenThrowBuildException() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.getSize());
  }

  /**
   * Test {@link URLResource#equals(Object)}, and {@link URLResource#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link URLResource#equals(Object)}
   *   <li>{@link URLResource#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    URLResource urlResource = new URLResource();
    URLResource urlResource2 = new URLResource();

    // Act and Assert
    assertEquals(urlResource, urlResource2);
    int expectedHashCodeResult = urlResource.hashCode();
    assertEquals(expectedHashCodeResult, urlResource2.hashCode());
  }

  /**
   * Test {@link URLResource#equals(Object)}, and {@link URLResource#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link URLResource#equals(Object)}
   *   <li>{@link URLResource#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    URLResource urlResource = new URLResource();

    // Act and Assert
    assertEquals(urlResource, urlResource);
    int expectedHashCodeResult = urlResource.hashCode();
    assertEquals(expectedHashCodeResult, urlResource.hashCode());
  }

  /**
   * Test {@link URLResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    URLResource urlResource = new URLResource("https://example.org/example");

    // Act and Assert
    assertNotEquals(urlResource, new URLResource());
  }

  /**
   * Test {@link URLResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    URLResource urlResource = new URLResource();

    // Act and Assert
    assertNotEquals(urlResource, new URLResource("https://example.org/example"));
  }

  /**
   * Test {@link URLResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual3() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setRelativePath("https://example.org/example");
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertNotEquals(urlResource, new URLResource());
  }

  /**
   * Test {@link URLResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then throw exception.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenThrowException() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.equals(new URLResource()));
  }

  /**
   * Test {@link URLResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then throw exception.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenThrowException2() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource();

    URLResource urlResource2 = new URLResource();
    urlResource2.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.equals(urlResource2));
  }

  /**
   * Test {@link URLResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then throw exception.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenThrowException3() throws MalformedURLException {
    // Arrange
    URLResource urlResource = new URLResource("https://example.org/example");

    URLResource urlResource2 = new URLResource();
    urlResource2.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.equals(urlResource2));
  }

  /**
   * Test {@link URLResource#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new URLResource(), null);
  }

  /**
   * Test {@link URLResource#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new URLResource(), "Different type to URLResource");
  }

  /**
   * Test {@link URLResource#getInputStream()}.
   * <p>
   * Method under test: {@link URLResource#getInputStream()}
   */
  @Test
  public void testGetInputStream() throws IOException {
    // Arrange
    URLResource urlResource = new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    urlResource.connect(1);

    // Act and Assert
    assertEquals(-1, urlResource.getInputStream().read(new byte[]{}));
  }

  /**
   * Test {@link URLResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenURLResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new URLResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).getInputStream()
            .read(new byte[]{}));
  }

  /**
   * Test {@link URLResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenURLResource_thenThrowBuildException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new URLResource()).getInputStream());
  }

  /**
   * Test {@link URLResource#getInputStream()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_thenThrowBuildException() throws IOException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.getInputStream());
  }

  /**
   * Test {@link URLResource#getOutputStream()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getOutputStream()}
   */
  @Test
  public void testGetOutputStream_givenURLResource_thenThrowBuildException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new URLResource()).getOutputStream());
  }

  /**
   * Test {@link URLResource#getOutputStream()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#getOutputStream()}
   */
  @Test
  public void testGetOutputStream_thenThrowBuildException() throws IOException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.getOutputStream());
  }

  /**
   * Test {@link URLResource#connect(int)} with {@code int}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#connect(int)}
   */
  @Test
  public void testConnectWithInt_givenURLResource_thenThrowBuildException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new URLResource()).connect(1));
  }

  /**
   * Test {@link URLResource#connect(int)} with {@code int}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#connect(int)}
   */
  @Test
  public void testConnectWithInt_thenThrowBuildException() throws IOException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.connect(1));
  }

  /**
   * Test {@link URLResource#connect()}.
   * <ul>
   *   <li>Given {@link URLResource#URLResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#connect()}
   */
  @Test
  public void testConnect_givenURLResource_thenThrowBuildException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new URLResource()).connect());
  }

  /**
   * Test {@link URLResource#connect()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link URLResource#connect()}
   */
  @Test
  public void testConnect_thenThrowBuildException() throws IOException {
    // Arrange
    URLResource urlResource = new URLResource();
    urlResource.setBaseURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());

    // Act and Assert
    assertThrows(BuildException.class, () -> urlResource.connect());
  }
}
