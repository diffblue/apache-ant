package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.tar.TarEntry;
import org.junit.Test;

public class TarResourceDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TarResource#TarResource()}
   *   <li>{@link TarResource#getLinkFlag()}
   *   <li>{@link TarResource#getLinkName()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    TarResource actualTarResource = new TarResource();
    byte actualLinkFlag = actualTarResource.getLinkFlag();

    // Assert
    assertEquals("", actualTarResource.getLinkName());
    Location location = actualTarResource.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTarResource.getDescription());
    assertNull(actualTarResource.getProject());
    assertNull(actualTarResource.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals('0', actualLinkFlag);
  }

  /**
   * Test {@link TarResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link JavaResource#JavaResource(String, Path)} with name is empty string and path is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenJavaResourceWithNameIsEmptyStringAndPathIsNull() throws IOException {
    // Arrange
    JavaResource a = new JavaResource("", null);

    // Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource(a, new TarEntry("Name"))).getInputStream());
  }

  /**
   * Test {@link TarResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenPropertyResourceProjectIsProject_thenThrowBuildException() throws IOException {
    // Arrange
    PropertyResource a = new PropertyResource();
    a.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource(a, new TarEntry("Name"))).getInputStream());
  }

  /**
   * Test {@link TarResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource(Resource, TarEntry)} with a is {@link PropertyResource#PropertyResource()} and e is {@link TarEntry#TarEntry(String)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenTarResourceWithAIsPropertyResourceAndEIsTarEntry() throws IOException {
    // Arrange
    PropertyResource a = new PropertyResource();

    // Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource(a, new TarEntry("Name"))).getInputStream());
  }

  /**
   * Test {@link TarResource#getOutputStream()}.
   * <p>
   * Method under test: {@link TarResource#getOutputStream()}
   */
  @Test
  public void testGetOutputStream() throws IOException {
    // Arrange, Act and Assert
    assertThrows(UnsupportedOperationException.class, () -> (new TarResource()).getOutputStream());
  }

  /**
   * Test {@link TarResource#getUserName()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with name is {@code entry name not set}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#getUserName()}
   */
  @Test
  public void testGetUserName_givenTarEntryWithNameIsEntryNameNotSet_thenReturnEmptyString() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals("", (new TarResource(a, new TarEntry("entry name not set"))).getUserName());
  }

  /**
   * Test {@link TarResource#getGroup()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with name is {@code entry name not set}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#getGroup()}
   */
  @Test
  public void testGetGroup_givenTarEntryWithNameIsEntryNameNotSet_thenReturnEmptyString() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals("", (new TarResource(a, new TarEntry("entry name not set"))).getGroup());
  }

  /**
   * Test {@link TarResource#getLongUid()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with name is {@code entry name not set}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#getLongUid()}
   */
  @Test
  public void testGetLongUid_givenTarEntryWithNameIsEntryNameNotSet_thenReturnZero() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0L, (new TarResource(a, new TarEntry("entry name not set"))).getLongUid());
  }

  /**
   * Test {@link TarResource#getUid()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with name is {@code entry name not set}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#getUid()}
   */
  @Test
  public void testGetUid_givenTarEntryWithNameIsEntryNameNotSet_thenReturnZero() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, (new TarResource(a, new TarEntry("entry name not set"))).getUid());
  }

  /**
   * Test {@link TarResource#getLongGid()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with name is {@code entry name not set}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#getLongGid()}
   */
  @Test
  public void testGetLongGid_givenTarEntryWithNameIsEntryNameNotSet_thenReturnZero() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0L, (new TarResource(a, new TarEntry("entry name not set"))).getLongGid());
  }

  /**
   * Test {@link TarResource#getGid()}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with name is {@code entry name not set}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#getGid()}
   */
  @Test
  public void testGetGid_givenTarEntryWithNameIsEntryNameNotSet_thenReturnZero() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0, (new TarResource(a, new TarEntry("entry name not set"))).getGid());
  }

  /**
   * Test {@link TarResource#fetchEntry()}.
   * <p>
   * Method under test: {@link TarResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource(a, new TarEntry("Name"))).fetchEntry());
  }

  /**
   * Test {@link TarResource#fetchEntry()}.
   * <p>
   * Method under test: {@link TarResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry2() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    TarResource tarResource = new TarResource(a, new TarEntry("Name"));
    tarResource.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.fetchEntry());
  }

  /**
   * Test {@link TarResource#fetchEntry()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry_givenFileResourceNameIsFileAttributeIsNull() {
    // Arrange
    FileResource a = new FileResource();
    a.setName("file attribute is null!");

    // Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource(a, new TarEntry("Name"))).fetchEntry());
  }

  /**
   * Test {@link TarResource#fetchEntry()}.
   * <ul>
   *   <li>Given {@link JavaResource#JavaResource(String, Path)} with {@code Name} and path is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry_givenJavaResourceWithNameAndPathIsNull_thenThrowBuildException() {
    // Arrange
    JavaResource a = new JavaResource("Name", null);

    // Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource(a, new TarEntry("Name"))).fetchEntry());
  }

  /**
   * Test {@link TarResource#fetchEntry()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    TarResource tarResource = new TarResource(a, new TarEntry("Name"));
    tarResource.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.fetchEntry());
  }

  /**
   * Test {@link TarResource#fetchEntry()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource(Resource, TarEntry)} with a is {@link JavaConstantResource} (default constructor) and e is {@link TarEntry#TarEntry(String)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry_givenTarResourceWithAIsJavaConstantResourceAndEIsTarEntry() {
    // Arrange
    JavaConstantResource a = new JavaConstantResource();

    // Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource(a, new TarEntry("Name"))).fetchEntry());
  }
}
