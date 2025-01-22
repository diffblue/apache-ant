package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Iterator;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.junit.Test;

public class FileResourceDiffblueTest {
  /**
   * Test {@link FileResource#FileResource(Project, String)}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then return not iterator hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#FileResource(Project, String)}
   */
  @Test
  public void testNewFileResource_whenDot_thenReturnNotIteratorHasNext() {
    // Arrange and Act
    FileResource actualFileResource = new FileResource(new Project(), ".");

    // Assert
    Iterator<Resource> iteratorResult = actualFileResource.iterator();
    Resource actualNextResult = iteratorResult.next();
    assertFalse(iteratorResult.hasNext());
    assertSame(actualFileResource, actualNextResult);
  }

  /**
   * Test {@link FileResource#FileResource(Project, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return not iterator hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#FileResource(Project, String)}
   */
  @Test
  public void testNewFileResource_whenEmptyString_thenReturnNotIteratorHasNext() {
    // Arrange and Act
    FileResource actualFileResource = new FileResource(new Project(), "");

    // Assert
    Iterator<Resource> iteratorResult = actualFileResource.iterator();
    Resource actualNextResult = iteratorResult.next();
    assertFalse(iteratorResult.hasNext());
    assertSame(actualFileResource, actualNextResult);
  }

  /**
   * Test {@link FileResource#FileResource(Project, String)}.
   * <ul>
   *   <li>When {@code foo}.</li>
   *   <li>Then return Name is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#FileResource(Project, String)}
   */
  @Test
  public void testNewFileResource_whenFoo_thenReturnNameIsFoo() {
    // Arrange and Act
    FileResource actualFileResource = new FileResource(new Project(), "foo");

    // Assert
    assertEquals("foo", actualFileResource.getName());
    assertEquals(0L, actualFileResource.getLastModified());
    assertEquals(0L, actualFileResource.getSize());
    assertFalse(actualFileResource.isDirectory());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("user.dir"), "foo").toString(), "\"");
    assertEquals(expectedToLongStringResult, actualFileResource.toLongString());
  }

  /**
   * Test {@link FileResource#setFile(File)}.
   * <p>
   * Method under test: {@link FileResource#setFile(File)}
   */
  @Test
  public void testSetFile() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    fileResource.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert that nothing has changed
    assertEquals("test.txt", fileResource.getName());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), "\"");
    assertEquals(expectedToLongStringResult, fileResource.toLongString());
  }

  /**
   * Test {@link FileResource#setFile(File)}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then {@link FileResource#FileResource()} BaseDir Absolute.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#setFile(File)}
   */
  @Test
  public void testSetFile_givenFileResource_thenFileResourceBaseDirAbsolute() {
    // Arrange
    FileResource fileResource = new FileResource();

    // Act
    fileResource.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertTrue(fileResource.getBaseDir().isAbsolute());
  }

  /**
   * Test {@link FileResource#getFile()}.
   * <p>
   * Method under test: {@link FileResource#getFile()}
   */
  @Test
  public void testGetFile() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act
    File actualFile = fileResource.getFile();

    // Assert
    assertEquals("test.txt", actualFile.getName());
    assertTrue(actualFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then return Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getFile()}
   */
  @Test
  public void testGetFile_givenFileResourceNameIsDotDot_thenReturnNameIsDownloads() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("..");

    // Act
    File actualFile = fileResource.getFile();

    // Assert
    assertEquals("Downloads", actualFile.getName());
    assertTrue(actualFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getFile()}
   */
  @Test
  public void testGetFile_givenFileResourceNameIsDot_thenReturnNameIsApacheAnt11015() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName(".");

    // Act
    File actualFile = fileResource.getFile();

    // Assert
    assertEquals("apache-ant-1.10.15", actualFile.getName());
    assertTrue(actualFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getFile()}
   */
  @Test
  public void testGetFile_givenFileResourceNameIsDot_thenReturnNameIsTestTxt() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(".");

    // Act
    File actualFile = fileResource.getFile();

    // Assert
    assertEquals("test.txt", actualFile.getName());
    assertTrue(actualFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is empty string.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getFile()}
   */
  @Test
  public void testGetFile_givenFileResourceNameIsEmptyString_thenReturnNameIsApacheAnt11015() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("");

    // Act
    File actualFile = fileResource.getFile();

    // Assert
    assertEquals("apache-ant-1.10.15", actualFile.getName());
    assertTrue(actualFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code Name}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getFile()}
   */
  @Test
  public void testGetFile_givenFileResourceNameIsName_thenReturnName() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("Name");

    // Act
    File actualFile = fileResource.getFile();

    // Assert
    assertEquals("Name", actualFile.getName());
    assertTrue(actualFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getFile()}
   */
  @Test
  public void testGetFile_givenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange and Act
    File actualFile = (new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()))
        .getFile();

    // Assert
    assertEquals("test.txt", actualFile.getName());
    assertTrue(actualFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getFile()}
   */
  @Test
  public void testGetFile_givenFileResource_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new FileResource()).getFile());
  }

  /**
   * Test {@link FileResource#setBaseDir(File)}.
   * <p>
   * Method under test: {@link FileResource#setBaseDir(File)}
   */
  @Test
  public void testSetBaseDir() {
    // Arrange
    FileResource fileResource = new FileResource();
    File b = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    fileResource.setBaseDir(b);

    // Assert
    assertSame(b, fileResource.getBaseDir());
  }

  /**
   * Test {@link FileResource#getBaseDir()}.
   * <p>
   * Method under test: {@link FileResource#getBaseDir()}
   */
  @Test
  public void testGetBaseDir() {
    // Arrange, Act and Assert
    assertNull((new FileResource()).getBaseDir());
  }

  /**
   * Test {@link FileResource#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then {@link FileResource#FileResource()} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenFileResource_thenFileResourceReference() {
    // Arrange
    FileResource fileResource = new FileResource();
    Reference r = new Reference("42");

    // Act
    fileResource.setRefid(r);

    // Assert
    assertTrue(fileResource.isReference());
    assertSame(r, fileResource.getRefid());
  }

  /**
   * Test {@link FileResource#getName()}.
   * <p>
   * Method under test: {@link FileResource#getName()}
   */
  @Test
  public void testGetName() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act and Assert
    assertEquals("", fileResource.getName());
  }

  /**
   * Test {@link FileResource#getName()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is empty string.</li>
   *   <li>Then return {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getName()}
   */
  @Test
  public void testGetName_givenFileResourceNameIsEmptyString_thenReturnApacheAnt11015() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("");

    // Act and Assert
    assertEquals("apache-ant-1.10.15", fileResource.getName());
  }

  /**
   * Test {@link FileResource#getName()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getName()}
   */
  @Test
  public void testGetName_givenFileResourceNameIsEmptyString_thenReturnEmptyString() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName("");

    // Act and Assert
    assertEquals("", fileResource.getName());
  }

  /**
   * Test {@link FileResource#getName()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code var}.</li>
   *   <li>Then return {@code var}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getName()}
   */
  @Test
  public void testGetName_givenFileResourceNameIsVar_thenReturnVar() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName("var");

    // Act and Assert
    assertEquals("var", fileResource.getName());
  }

  /**
   * Test {@link FileResource#getName()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getName()}
   */
  @Test
  public void testGetName_givenFileResourceWithFIsPropertyIsJavaIoTmpdirIsDotToFile() {
    // Arrange, Act and Assert
    assertEquals("", (new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile())).getName());
  }

  /**
   * Test {@link FileResource#getName()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getName()}
   */
  @Test
  public void testGetName_givenFileResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FileResource()).getName());
  }

  /**
   * Test {@link FileResource#getName()}.
   * <ul>
   *   <li>Then return {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getName()}
   */
  @Test
  public void testGetName_thenReturnFileAttributeIsNull() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertEquals("file attribute is null!", fileResource.getName());
  }

  /**
   * Test {@link FileResource#getName()}.
   * <ul>
   *   <li>Then return {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getName()}
   */
  @Test
  public void testGetName_thenReturnTestTxt() {
    // Arrange, Act and Assert
    assertEquals("test.txt",
        (new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).getName());
  }

  /**
   * Test {@link FileResource#getName()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getName()}
   */
  @Test
  public void testGetName_thenThrowBuildException() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> fileResource.getName());
  }

  /**
   * Test {@link FileResource#isExists()}.
   * <p>
   * Method under test: {@link FileResource#isExists()}
   */
  @Test
  public void testIsExists() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act and Assert
    assertTrue(fileResource.isExists());
  }

  /**
   * Test {@link FileResource#isExists()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isExists()}
   */
  @Test
  public void testIsExists_givenFileResourceNameIsDotDot_thenReturnTrue() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("..");

    // Act and Assert
    assertTrue(fileResource.isExists());
  }

  /**
   * Test {@link FileResource#isExists()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isExists()}
   */
  @Test
  public void testIsExists_givenFileResourceNameIsDotDot_thenReturnTrue2() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName("..");

    // Act and Assert
    assertTrue(fileResource.isExists());
  }

  /**
   * Test {@link FileResource#isExists()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isExists()}
   */
  @Test
  public void testIsExists_givenFileResourceNameIsDot_thenReturnTrue() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName(".");

    // Act and Assert
    assertTrue(fileResource.isExists());
  }

  /**
   * Test {@link FileResource#isExists()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isExists()}
   */
  @Test
  public void testIsExists_givenFileResourceNameIsDot_thenReturnTrue2() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(".");

    // Act and Assert
    assertTrue(fileResource.isExists());
  }

  /**
   * Test {@link FileResource#isExists()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isExists()}
   */
  @Test
  public void testIsExists_givenFileResourceNameIsEmptyString_thenReturnTrue() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("");

    // Act and Assert
    assertTrue(fileResource.isExists());
  }

  /**
   * Test {@link FileResource#isExists()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isExists()}
   */
  @Test
  public void testIsExists_givenFileResourceNameIsFileAttributeIsNull_thenReturnFalse() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertFalse(fileResource.isExists());
  }

  /**
   * Test {@link FileResource#isExists()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isExists()}
   */
  @Test
  public void testIsExists_givenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange, Act and Assert
    assertTrue((new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).isExists());
  }

  /**
   * Test {@link FileResource#isExists()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isExists()}
   */
  @Test
  public void testIsExists_givenFileResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FileResource()).isExists());
  }

  /**
   * Test {@link FileResource#getLastModified()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenFileResourceNameIsFileAttributeIsNull_thenReturnZero() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertEquals(0L, fileResource.getLastModified());
  }

  /**
   * Test {@link FileResource#getLastModified()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenFileResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FileResource()).getLastModified());
  }

  /**
   * Test {@link FileResource#isDirectory()}.
   * <p>
   * Method under test: {@link FileResource#isDirectory()}
   */
  @Test
  public void testIsDirectory() {
    // Arrange, Act and Assert
    assertFalse((new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "file attribute is null!")).isDirectory());
  }

  /**
   * Test {@link FileResource#isDirectory()}.
   * <p>
   * Method under test: {@link FileResource#isDirectory()}
   */
  @Test
  public void testIsDirectory2() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act and Assert
    assertTrue(fileResource.isDirectory());
  }

  /**
   * Test {@link FileResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenFileResourceNameIsDotDot_thenReturnTrue() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("..");

    // Act and Assert
    assertTrue(fileResource.isDirectory());
  }

  /**
   * Test {@link FileResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenFileResourceNameIsDotDot_thenReturnTrue2() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName("..");

    // Act and Assert
    assertTrue(fileResource.isDirectory());
  }

  /**
   * Test {@link FileResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenFileResourceNameIsDot_thenReturnTrue() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName(".");

    // Act and Assert
    assertTrue(fileResource.isDirectory());
  }

  /**
   * Test {@link FileResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenFileResourceNameIsDot_thenReturnTrue2() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(".");

    // Act and Assert
    assertTrue(fileResource.isDirectory());
  }

  /**
   * Test {@link FileResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenFileResourceNameIsEmptyString_thenReturnTrue() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("");

    // Act and Assert
    assertTrue(fileResource.isDirectory());
  }

  /**
   * Test {@link FileResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenFileResourceNameIsFileAttributeIsNull_thenReturnFalse() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertFalse(fileResource.isDirectory());
  }

  /**
   * Test {@link FileResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange, Act and Assert
    assertTrue((new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).isDirectory());
  }

  /**
   * Test {@link FileResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenFileResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FileResource()).isDirectory());
  }

  /**
   * Test {@link FileResource#getSize()}.
   * <p>
   * Method under test: {@link FileResource#getSize()}
   */
  @Test
  public void testGetSize() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act and Assert
    assertEquals(64L, fileResource.getSize());
  }

  /**
   * Test {@link FileResource#getSize()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return sixty-four.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getSize()}
   */
  @Test
  public void testGetSize_givenFileResourceNameIsDot_thenReturnSixtyFour() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(".");

    // Act and Assert
    assertEquals(64L, fileResource.getSize());
  }

  /**
   * Test {@link FileResource#getSize()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getSize()}
   */
  @Test
  public void testGetSize_givenFileResourceNameIsFileAttributeIsNull_thenReturnZero() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertEquals(0L, fileResource.getSize());
  }

  /**
   * Test {@link FileResource#getSize()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getSize()}
   */
  @Test
  public void testGetSize_givenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange, Act and Assert
    assertEquals(64L,
        (new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).getSize());
  }

  /**
   * Test {@link FileResource#getSize()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getSize()}
   */
  @Test
  public void testGetSize_givenFileResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FileResource()).getSize());
  }

  /**
   * Test {@link FileResource#getInputStream()}.
   * <p>
   * Method under test: {@link FileResource#getInputStream()}
   */
  @Test
  public void testGetInputStream() throws IOException {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act and Assert
    assertEquals(Double.SIZE, fileResource.getInputStream().available());
  }

  /**
   * Test {@link FileResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return available is {@link Double#SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenFileResourceNameIsDot_thenReturnAvailableIsSize() throws IOException {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(".");

    // Act and Assert
    assertEquals(Double.SIZE, fileResource.getInputStream().available());
  }

  /**
   * Test {@link FileResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() throws IOException {
    // Arrange, Act and Assert
    assertEquals(Double.SIZE,
        (new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())).getInputStream()
            .available());
  }

  /**
   * Test {@link FileResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenFileResource_thenThrowBuildException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FileResource()).getInputStream());
  }

  /**
   * Test {@link FileResource#getOutputStream()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getOutputStream()}
   */
  @Test
  public void testGetOutputStream_givenFileResource_thenThrowBuildException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FileResource()).getOutputStream());
  }

  /**
   * Test {@link FileResource#getAppendOutputStream()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getAppendOutputStream()}
   */
  @Test
  public void testGetAppendOutputStream_givenFileResource_thenThrowBuildException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FileResource()).getAppendOutputStream());
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> fileResource.compareTo(new Resource()));
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource2() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "file attribute is null!");
    FileResource r = new FileResource();

    // Act and Assert
    assertThrows(BuildException.class, () -> fileResource.compareTo(new MappedResource(r, new CutDirsMapper())));
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_givenFileResource_whenFileResource_thenReturnZero() {
    // Arrange
    FileResource fileResource = new FileResource();

    // Act and Assert
    assertEquals(0, fileResource.compareTo(new FileResource()));
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_givenFileResource_whenResource_thenThrowBuildException() {
    // Arrange
    FileResource fileResource = new FileResource();

    // Act and Assert
    assertThrows(BuildException.class, () -> fileResource.compareTo(new Resource()));
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return eight.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnEight() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(8, fileResource
        .compareTo(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "")));
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return fourteen.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnFourteen() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(14, fileResource.compareTo(new Resource("file attribute is null!")));
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return minus fifty-five.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnMinusFiftyFive() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile());

    // Act and Assert
    assertEquals(-55, fileResource.compareTo(new Resource("file attribute is null!")));
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnMinusOne() {
    // Arrange
    FileResource fileResource = new FileResource();

    // Act and Assert
    assertEquals(-1,
        fileResource.compareTo(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return minus twenty-four.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnMinusTwentyFour() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(-24,
        fileResource.compareTo(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
            "file attribute is null!")));
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnZero() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(0,
        fileResource.compareTo(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link FileResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_whenFileResource_thenReturnOne() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(1, fileResource.compareTo(new FileResource()));
  }

  /**
   * Test {@link FileResource#equals(Object)}, and {@link FileResource#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link FileResource#equals(Object)}
   *   <li>{@link FileResource#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    FileResource fileResource = new FileResource();
    FileResource fileResource2 = new FileResource();

    // Act and Assert
    assertEquals(fileResource, fileResource2);
    int expectedHashCodeResult = fileResource.hashCode();
    assertEquals(expectedHashCodeResult, fileResource2.hashCode());
  }

  /**
   * Test {@link FileResource#equals(Object)}, and {@link FileResource#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link FileResource#equals(Object)}
   *   <li>{@link FileResource#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual2() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    FileResource fileResource2 = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(fileResource, fileResource2);
    int expectedHashCodeResult = fileResource.hashCode();
    assertEquals(expectedHashCodeResult, fileResource2.hashCode());
  }

  /**
   * Test {@link FileResource#equals(Object)}, and {@link FileResource#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link FileResource#equals(Object)}
   *   <li>{@link FileResource#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    FileResource fileResource = new FileResource();

    // Act and Assert
    assertEquals(fileResource, fileResource);
    int expectedHashCodeResult = fileResource.hashCode();
    assertEquals(expectedHashCodeResult, fileResource.hashCode());
  }

  /**
   * Test {@link FileResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNotEquals(fileResource, new FileResource());
  }

  /**
   * Test {@link FileResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    FileResource fileResource = new FileResource();

    // Act and Assert
    assertNotEquals(fileResource,
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual3() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("Name");

    // Act and Assert
    assertNotEquals(fileResource, new FileResource());
  }

  /**
   * Test {@link FileResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual4() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNotEquals(fileResource,
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual5() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName("Name");

    // Act and Assert
    assertNotEquals(fileResource, new FileResource());
  }

  /**
   * Test {@link FileResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual6() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName("..");

    // Act and Assert
    assertNotEquals(fileResource, new FileResource());
  }

  /**
   * Test {@link FileResource#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new FileResource(), null);
  }

  /**
   * Test {@link FileResource#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new FileResource(), "Different type to FileResource");
  }

  /**
   * Test {@link FileResource#toString()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code (unbound file resource)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#toString()}
   */
  @Test
  public void testToString_givenFileResource_thenReturnUnboundFileResource() {
    // Arrange, Act and Assert
    assertEquals("(unbound file resource)", (new FileResource()).toString());
  }

  /**
   * Test {@link FileResource#toString()}.
   * <ul>
   *   <li>Then return Property is {@code java.io.tmpdir} is array of {@link String} with {@code test.txt} toString.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#toString()}
   */
  @Test
  public void testToString_thenReturnPropertyIsJavaIoTmpdirIsArrayOfStringWithTestTxtToString() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    String actualToStringResult = fileResource.toString();

    // Assert
    assertEquals(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), actualToStringResult);
  }

  /**
   * Test {@link FileResource#isFilesystemOnly()}.
   * <p>
   * Method under test: {@link FileResource#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly() {
    // Arrange, Act and Assert
    assertTrue((new FileResource()).isFilesystemOnly());
  }

  /**
   * Test {@link FileResource#touch(long)}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#touch(long)}
   */
  @Test
  public void testTouch_givenFileResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FileResource()).touch(1L));
  }

  /**
   * Test {@link FileResource#getNotNullFile()}.
   * <p>
   * Method under test: {@link FileResource#getNotNullFile()}
   */
  @Test
  public void testGetNotNullFile() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act
    File actualNotNullFile = fileResource.getNotNullFile();

    // Assert
    assertEquals("test.txt", actualNotNullFile.getName());
    assertTrue(actualNotNullFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getNotNullFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then return Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getNotNullFile()}
   */
  @Test
  public void testGetNotNullFile_givenFileResourceNameIsDotDot_thenReturnNameIsDownloads() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("..");

    // Act
    File actualNotNullFile = fileResource.getNotNullFile();

    // Assert
    assertEquals("Downloads", actualNotNullFile.getName());
    assertTrue(actualNotNullFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getNotNullFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getNotNullFile()}
   */
  @Test
  public void testGetNotNullFile_givenFileResourceNameIsDot_thenReturnNameIsApacheAnt11015() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName(".");

    // Act
    File actualNotNullFile = fileResource.getNotNullFile();

    // Assert
    assertEquals("apache-ant-1.10.15", actualNotNullFile.getName());
    assertTrue(actualNotNullFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getNotNullFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getNotNullFile()}
   */
  @Test
  public void testGetNotNullFile_givenFileResourceNameIsDot_thenReturnNameIsTestTxt() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(".");

    // Act
    File actualNotNullFile = fileResource.getNotNullFile();

    // Assert
    assertEquals("test.txt", actualNotNullFile.getName());
    assertTrue(actualNotNullFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getNotNullFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getNotNullFile()}
   */
  @Test
  public void testGetNotNullFile_givenFileResourceNameIsEmptyString() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("");

    // Act
    File actualNotNullFile = fileResource.getNotNullFile();

    // Assert
    assertEquals("apache-ant-1.10.15", actualNotNullFile.getName());
    assertTrue(actualNotNullFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getNotNullFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getNotNullFile()}
   */
  @Test
  public void testGetNotNullFile_givenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange and Act
    File actualNotNullFile = (new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()))
        .getNotNullFile();

    // Assert
    assertEquals("test.txt", actualNotNullFile.getName());
    assertTrue(actualNotNullFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getNotNullFile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getNotNullFile()}
   */
  @Test
  public void testGetNotNullFile_givenFileResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new FileResource()).getNotNullFile());
  }

  /**
   * Test {@link FileResource#getNotNullFile()}.
   * <ul>
   *   <li>Then return Name is {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getNotNullFile()}
   */
  @Test
  public void testGetNotNullFile_thenReturnNameIsFileAttributeIsNull() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act
    File actualNotNullFile = fileResource.getNotNullFile();

    // Assert
    assertEquals("file attribute is null!", actualNotNullFile.getName());
    assertTrue(actualNotNullFile.isAbsolute());
  }

  /**
   * Test {@link FileResource#getResource(String)}.
   * <p>
   * Method under test: {@link FileResource#getResource(String)}
   */
  @Test
  public void testGetResource() {
    // Arrange
    FileResource fileResource = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    Resource actualResource = fileResource.getResource("");

    // Assert
    assertTrue(actualResource instanceof FileResource);
    assertEquals(fileResource, actualResource);
  }

  /**
   * Test {@link FileResource#getResource(String)}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return BaseDir Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResource#getResource(String)}
   */
  @Test
  public void testGetResource_givenFileResourceNameIsDot_thenReturnBaseDirNameIsTestTxt() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    fileResource.setName(".");

    // Act
    Resource actualResource = fileResource.getResource("Path");

    // Assert
    assertTrue(actualResource instanceof FileResource);
    assertEquals("Path", actualResource.getName());
    assertEquals("test.txt", ((FileResource) actualResource).getBaseDir().getName());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt", "Path").toString(), "\"");
    assertEquals(expectedToLongStringResult, actualResource.toLongString());
  }
}
