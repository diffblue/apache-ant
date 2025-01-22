package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class PackDiffblueTest {
  /**
   * Test {@link Pack#setZipfile(File)}.
   * <p>
   * Method under test: {@link Pack#setZipfile(File)}
   */
  @Test
  public void testSetZipfile() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    // Act
    bZip2.setZipfile(Copy.NULL_FILE_PLACEHOLDER);

    // Assert
    File file = bZip2.zipFile;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Pack#setDestfile(File)}.
   * <p>
   * Method under test: {@link Pack#setDestfile(File)}
   */
  @Test
  public void testSetDestfile() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    // Act
    bZip2.setDestfile(Copy.NULL_FILE_PLACEHOLDER);

    // Assert
    File file = bZip2.zipFile;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Pack#setSrc(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrc(File)}
   */
  @Test
  public void testSetSrc_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenThrowBuildException() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> bZip2.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource() {
    // Arrange
    BZip2 bZip2 = new BZip2();
    FileResource src = new FileResource(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    bZip2.setSrcResource(src);

    // Assert
    File file = bZip2.source;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
    assertSame(src, bZip2.getSrcResource());
    File expectedFile = bZip2.source;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@link BZip2} (default constructor).</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then {@link BZip2} (default constructor) SrcResource is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenBZip2_whenResource_thenBZip2SrcResourceIsResource() {
    // Arrange
    BZip2 bZip2 = new BZip2();
    Resource src = new Resource();

    // Act
    bZip2.setSrcResource(src);

    // Assert
    assertSame(src, bZip2.getSrcResource());
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenDotDot_whenFileResourceNameIsDotDot() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    FileResource src = new FileResource();
    src.setName("..");

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2.setSrcResource(src));
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenDotDot_whenFileResourceNameIsDotDot2() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    FileResource src = new FileResource();
    src.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    src.setName("..");

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2.setSrcResource(src));
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenDot_whenFileResourceNameIsDot() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    FileResource src = new FileResource();
    src.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    src.setName(".");

    // Act
    bZip2.setSrcResource(src);

    // Assert
    File file = bZip2.source;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
    assertSame(src, bZip2.getSrcResource());
    File expectedFile = bZip2.source;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenDot_whenFileResourceNameIsDot_thenThrowBuildException() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    FileResource src = new FileResource();
    src.setName(".");

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2.setSrcResource(src));
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    FileResource src = new FileResource();
    src.setName("");

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2.setSrcResource(src));
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code /NULL_FILE}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code /NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenNullFile_whenFileResourceNameIsNullFile() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    FileResource src = new FileResource();
    src.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    src.setName("/NULL_FILE");

    // Act
    bZip2.setSrcResource(src);

    // Assert
    File file = bZip2.source;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
    assertSame(src, bZip2.getSrcResource());
    File expectedFile = bZip2.source;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Directory is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenTrue_whenResourceDirectoryIsTrue_thenThrowBuildException() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    Resource src = new Resource();
    src.setDirectory(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2.setSrcResource(src));
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Then {@link BZip2} (default constructor) {@link Pack#source} Name is {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_thenBZip2SourceNameIsFileAttributeIsNull() {
    // Arrange
    BZip2 bZip2 = new BZip2();

    FileResource src = new FileResource();
    src.setName("file attribute is null!");

    // Act
    bZip2.setSrcResource(src);

    // Assert
    File file = bZip2.source;
    assertEquals("file attribute is null!", file.getName());
    assertTrue(file.isAbsolute());
    assertSame(src, bZip2.getSrcResource());
    File expectedFile = bZip2.source;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link Pack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Then {@link BZip2} (default constructor) SrcResource is {@link Resource#Resource(String)} with name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_thenBZip2SrcResourceIsResourceWithNameIsAttribute_name() {
    // Arrange
    BZip2 bZip2 = new BZip2();
    Resource src = new Resource(Manifest.ATTRIBUTE_NAME);

    // Act
    bZip2.setSrcResource(src);

    // Assert
    assertSame(src, bZip2.getSrcResource());
  }

  /**
   * Test {@link Pack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenNone_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2()).addConfigured(Resources.NONE));
  }

  /**
   * Test {@link Pack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#systemBootClasspath}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenSystemBootClasspath_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2()).addConfigured(Path.systemBootClasspath));
  }

  /**
   * Test {@link Pack#execute()}.
   * <p>
   * Method under test: {@link Pack#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    BZip2 bZip2 = new BZip2();
    bZip2.setZipfile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2.execute());
  }

  /**
   * Test {@link Pack#execute()}.
   * <ul>
   *   <li>Given {@link BZip2} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#execute()}
   */
  @Test
  public void testExecute_givenBZip2() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BZip2()).execute());
  }

  /**
   * Test {@link Pack#execute()}.
   * <ul>
   *   <li>Given {@link BZip2} (default constructor) Zipfile is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Pack#execute()}
   */
  @Test
  public void testExecute_givenBZip2ZipfileIsNull_file_placeholder() throws BuildException {
    // Arrange
    BZip2 bZip2 = new BZip2();
    bZip2.setZipfile(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> bZip2.execute());
  }

  /**
   * Test {@link Pack#getSrcResource()}.
   * <p>
   * Method under test: {@link Pack#getSrcResource()}
   */
  @Test
  public void testGetSrcResource() {
    // Arrange, Act and Assert
    assertNull((new BZip2()).getSrcResource());
  }

  /**
   * Test {@link Pack#supportsNonFileResources()}.
   * <p>
   * Method under test: {@link Pack#supportsNonFileResources()}
   */
  @Test
  public void testSupportsNonFileResources() {
    // Arrange, Act and Assert
    assertTrue((new BZip2()).supportsNonFileResources());
  }
}
