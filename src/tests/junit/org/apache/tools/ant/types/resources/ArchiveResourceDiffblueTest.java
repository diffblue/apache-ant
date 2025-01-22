package org.apache.tools.ant.types.resources;

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
import org.apache.tools.ant.Project;
import org.apache.tools.ant.filters.ExpandProperties;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.tar.TarEntry;
import org.junit.Test;

public class ArchiveResourceDiffblueTest {
  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> (new TarResource(a, new TarEntry("only single argument resource collections are supported as archives")))
            .addConfigured(Resources.NONE));
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured2() {
    // Arrange
    TarResource tarResource = new TarResource();

    Concat a = new Concat();
    a.addText("At least one resource must be provided, or some text.");

    // Act
    tarResource.addConfigured(a);

    // Assert
    Resource archive = tarResource.getArchive();
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        archive.toLongString());
    assertEquals("TarResource \"concat (At least one resource must be provided, or some text.):null\"",
        tarResource.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", archive.getName());
    assertEquals(-1L, archive.getSize());
    assertEquals(0L, archive.getLastModified());
    assertFalse(archive.isFilesystemOnly());
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured3() {
    // Arrange
    TarResource tarResource = new TarResource();

    FileName name = new FileName();
    name.setName(".");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat a = new Concat();
    a.addFilelist(list);

    // Act
    tarResource.addConfigured(a);

    // Assert
    Resource archive = tarResource.getArchive();
    assertEquals(-1L, archive.getSize());
    assertEquals(0L, archive.getLastModified());
    assertFalse(archive.isFilesystemOnly());
    String expectedToLongStringResult = String.join("", "Concat$ConcatResource \"concat (",
        System.getProperty("user.dir"), ")\"");
    assertEquals(expectedToLongStringResult, archive.toLongString());
    String expectedToLongStringResult2 = String.join("", "TarResource \"concat (", System.getProperty("user.dir"),
        "):null\"");
    assertEquals(expectedToLongStringResult2, tarResource.toLongString());
    String expectedName = String.join("", "concat (", System.getProperty("user.dir"), ")");
    assertEquals(expectedName, archive.getName());
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured4() {
    // Arrange
    TarResource tarResource = new TarResource();

    FileName name = new FileName();
    name.setName("..");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat a = new Concat();
    a.addFilelist(list);

    // Act
    tarResource.addConfigured(a);

    // Assert
    Resource archive = tarResource.getArchive();
    assertEquals(-1L, archive.getSize());
    assertEquals(0L, archive.getLastModified());
    assertFalse(archive.isFilesystemOnly());
    String expectedToLongStringResult = String.join("", "Concat$ConcatResource \"concat (",
        Paths.get(System.getProperty("user.home"), "Downloads").toString(), ")\"");
    assertEquals(expectedToLongStringResult, archive.toLongString());
    String expectedToLongStringResult2 = String.join("", "TarResource \"concat (",
        Paths.get(System.getProperty("user.home"), "Downloads").toString(), "):null\"");
    assertEquals(expectedToLongStringResult2, tarResource.toLongString());
    String expectedName = String.join("", "concat (",
        Paths.get(System.getProperty("user.home"), "Downloads").toString(), ")");
    assertEquals(expectedName, archive.getName());
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileList() {
    // Arrange
    TarResource tarResource = new TarResource();

    Concat a = new Concat();
    a.addFilelist(new FileList());

    // Act
    tarResource.addConfigured(a);

    // Assert
    Resource archive = tarResource.getArchive();
    assertEquals("Concat$ConcatResource \"concat ()\"", archive.toLongString());
    assertEquals("TarResource \"concat ():null\"", tarResource.toLongString());
    assertEquals("concat ()", archive.getName());
    assertEquals(-1L, archive.getSize());
    assertEquals(0L, archive.getLastModified());
    assertFalse(archive.isFilesystemOnly());
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsName() {
    // Arrange
    TarResource tarResource = new TarResource();

    FileName name = new FileName();
    name.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat a = new Concat();
    a.addFilelist(list);

    // Act
    tarResource.addConfigured(a);

    // Assert
    Resource archive = tarResource.getArchive();
    assertEquals("Concat$ConcatResource \"concat ()\"", archive.toLongString());
    assertEquals("TarResource \"concat ():null\"", tarResource.toLongString());
    assertEquals("concat ()", archive.getName());
    assertEquals(-1L, archive.getSize());
    assertEquals(0L, archive.getLastModified());
    assertFalse(archive.isFilesystemOnly());
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenResource_whenConcatDestIsResource() {
    // Arrange
    TarResource tarResource = new TarResource();

    Concat a = new Concat();
    a.setDest(new Resource());
    a.addFilelist(new FileList());

    // Act
    tarResource.addConfigured(a);

    // Assert
    Resource archive = tarResource.getArchive();
    assertEquals("Concat$ConcatResource \"concat ()\"", archive.toLongString());
    assertEquals("TarResource \"concat ():null\"", tarResource.toLongString());
    assertEquals("concat ()", archive.getName());
    assertEquals(-1L, archive.getSize());
    assertEquals(0L, archive.getLastModified());
    assertFalse(archive.isFilesystemOnly());
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()}.</li>
   *   <li>When {@link Archives} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenTarResource_whenArchives_thenThrowBuildException() {
    // Arrange
    TarResource tarResource = new TarResource();

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.addConfigured(new Archives()));
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()}.</li>
   *   <li>When {@link Files#Files()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenTarResource_whenFiles_thenThrowBuildException() {
    // Arrange
    TarResource tarResource = new TarResource();

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.addConfigured(new Files()));
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()}.</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenTarResource_whenNone_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource()).addConfigured(Resources.NONE));
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then {@link TarResource#TarResource()} Archive is {@link BZip2Resource#BZip2Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenBZip2Resource_thenTarResourceArchiveIsBZip2Resource() {
    // Arrange
    TarResource tarResource = new TarResource();
    BZip2Resource a = new BZip2Resource();

    // Act
    tarResource.addConfigured(a);

    // Assert
    assertSame(a, tarResource.getArchive());
  }

  /**
   * Test {@link ArchiveResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenConcatProjectIsProject() {
    // Arrange
    TarResource tarResource = new TarResource();

    Concat a = new Concat();
    a.setProject(new Project());
    a.addText("At least one resource must be provided, or some text.");

    // Act
    tarResource.addConfigured(a);

    // Assert
    Resource archive = tarResource.getArchive();
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        archive.toLongString());
    assertEquals("TarResource \"concat (At least one resource must be provided, or some text.):null\"",
        tarResource.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", archive.getName());
    assertEquals(-1L, archive.getSize());
    assertEquals(0L, archive.getLastModified());
    assertFalse(archive.isFilesystemOnly());
  }

  /**
   * Test {@link ArchiveResource#getArchive()}.
   * <p>
   * Method under test: {@link ArchiveResource#getArchive()}
   */
  @Test
  public void testGetArchive() {
    // Arrange, Act and Assert
    assertNull((new TarResource()).getArchive());
  }

  /**
   * Test {@link ArchiveResource#getLastModified()}.
   * <p>
   * Method under test: {@link ArchiveResource#getLastModified()}
   */
  @Test
  public void testGetLastModified() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("entry name not set"));

    // Act
    tarResource.getLastModified();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, tarResource.getSize());
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
    assertEquals(33188, tarResource.getMode());
  }

  /**
   * Test {@link ArchiveResource#getLastModified()}.
   * <p>
   * Method under test: {@link ArchiveResource#getLastModified()}
   */
  @Test
  public void testGetLastModified2() {
    // Arrange
    TarResource tarResource = new TarResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        null);

    // Act
    long actualLastModified = tarResource.getLastModified();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getMode());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, actualLastModified);
    assertEquals(0L, tarResource.getSize());
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
  }

  /**
   * Test {@link ArchiveResource#getLastModified()}.
   * <p>
   * Method under test: {@link ArchiveResource#getLastModified()}
   */
  @Test
  public void testGetLastModified3() {
    // Arrange
    TarResource tarResource = new TarResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        null);
    tarResource.setExists(true);

    // Act
    long actualLastModified = tarResource.getLastModified();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(-1L, tarResource.getSize());
    assertEquals(0, tarResource.getMode());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, actualLastModified);
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
  }

  /**
   * Test {@link ArchiveResource#getLastModified()}.
   * <p>
   * Method under test: {@link ArchiveResource#getLastModified()}
   */
  @Test
  public void testGetLastModified4() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getLastModified());
  }

  /**
   * Test {@link ArchiveResource#getLastModified()}.
   * <p>
   * Method under test: {@link ArchiveResource#getLastModified()}
   */
  @Test
  public void testGetLastModified5() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getLastModified());
  }

  /**
   * Test {@link ArchiveResource#getLastModified()}.
   * <p>
   * Method under test: {@link ArchiveResource#getLastModified()}
   */
  @Test
  public void testGetLastModified6() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getLastModified());
  }

  /**
   * Test {@link ArchiveResource#getLastModified()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenTarResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource()).getLastModified());
  }

  /**
   * Test {@link ArchiveResource#getLastModified()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#getLastModified()}
   */
  @Test
  public void testGetLastModified_thenThrowBuildException() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getLastModified());
  }

  /**
   * Test {@link ArchiveResource#getSize()}.
   * <p>
   * Method under test: {@link ArchiveResource#getSize()}
   */
  @Test
  public void testGetSize() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("entry name not set"));

    // Act
    long actualSize = tarResource.getSize();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, actualSize);
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
    assertEquals(33188, tarResource.getMode());
  }

  /**
   * Test {@link ArchiveResource#getSize()}.
   * <p>
   * Method under test: {@link ArchiveResource#getSize()}
   */
  @Test
  public void testGetSize2() {
    // Arrange
    TarResource tarResource = new TarResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        null);

    // Act
    long actualSize = tarResource.getSize();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getMode());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, tarResource.getLastModified());
    assertEquals(0L, actualSize);
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
  }

  /**
   * Test {@link ArchiveResource#getSize()}.
   * <p>
   * Method under test: {@link ArchiveResource#getSize()}
   */
  @Test
  public void testGetSize3() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getSize());
  }

  /**
   * Test {@link ArchiveResource#getSize()}.
   * <p>
   * Method under test: {@link ArchiveResource#getSize()}
   */
  @Test
  public void testGetSize4() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getSize());
  }

  /**
   * Test {@link ArchiveResource#getSize()}.
   * <p>
   * Method under test: {@link ArchiveResource#getSize()}
   */
  @Test
  public void testGetSize5() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getSize());
  }

  /**
   * Test {@link ArchiveResource#getSize()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()} Name is {@code entry name not set}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#getSize()}
   */
  @Test
  public void testGetSize_givenTarResourceNameIsEntryNameNotSet_thenThrowBuildException() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getSize());
  }

  /**
   * Test {@link ArchiveResource#getSize()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#getSize()}
   */
  @Test
  public void testGetSize_givenTarResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource()).getSize());
  }

  /**
   * Test {@link ArchiveResource#getSize()}.
   * <ul>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#getSize()}
   */
  @Test
  public void testGetSize_thenReturnMinusOne() {
    // Arrange
    TarResource tarResource = new TarResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        null);
    tarResource.setExists(true);

    // Act
    long actualSize = tarResource.getSize();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(-1L, actualSize);
    assertEquals(0, tarResource.getMode());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, tarResource.getLastModified());
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
  }

  /**
   * Test {@link ArchiveResource#isDirectory()}.
   * <p>
   * Method under test: {@link ArchiveResource#isDirectory()}
   */
  @Test
  public void testIsDirectory() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("entry name not set"));

    // Act
    boolean actualIsDirectoryResult = tarResource.isDirectory();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, tarResource.getSize());
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
    assertEquals(33188, tarResource.getMode());
    assertFalse(actualIsDirectoryResult);
  }

  /**
   * Test {@link ArchiveResource#isDirectory()}.
   * <p>
   * Method under test: {@link ArchiveResource#isDirectory()}
   */
  @Test
  public void testIsDirectory2() {
    // Arrange
    TarResource tarResource = new TarResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        null);

    // Act
    boolean actualIsDirectoryResult = tarResource.isDirectory();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getMode());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, tarResource.getLastModified());
    assertEquals(0L, tarResource.getSize());
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
    assertFalse(actualIsDirectoryResult);
  }

  /**
   * Test {@link ArchiveResource#isDirectory()}.
   * <p>
   * Method under test: {@link ArchiveResource#isDirectory()}
   */
  @Test
  public void testIsDirectory3() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.isDirectory());
  }

  /**
   * Test {@link ArchiveResource#isDirectory()}.
   * <p>
   * Method under test: {@link ArchiveResource#isDirectory()}
   */
  @Test
  public void testIsDirectory4() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.isDirectory());
  }

  /**
   * Test {@link ArchiveResource#isDirectory()}.
   * <p>
   * Method under test: {@link ArchiveResource#isDirectory()}
   */
  @Test
  public void testIsDirectory5() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.isDirectory());
  }

  /**
   * Test {@link ArchiveResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()} Name is {@code entry name not set}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenTarResourceNameIsEntryNameNotSet_thenThrowBuildException() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.isDirectory());
  }

  /**
   * Test {@link ArchiveResource#isDirectory()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenTarResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource()).isDirectory());
  }

  /**
   * Test {@link ArchiveResource#isDirectory()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#isDirectory()}
   */
  @Test
  public void testIsDirectory_thenReturnTrue() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    TarResource tarResource = new TarResource(a, new TarEntry("entry name not set"));
    tarResource.setDirectory(true);

    // Act
    boolean actualIsDirectoryResult = tarResource.isDirectory();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, tarResource.getSize());
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
    assertEquals(33188, tarResource.getMode());
    assertTrue(actualIsDirectoryResult);
  }

  /**
   * Test {@link ArchiveResource#isExists()}.
   * <p>
   * Method under test: {@link ArchiveResource#isExists()}
   */
  @Test
  public void testIsExists() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("entry name not set"));

    // Act
    boolean actualIsExistsResult = tarResource.isExists();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, tarResource.getSize());
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
    assertEquals(33188, tarResource.getMode());
    assertTrue(actualIsExistsResult);
  }

  /**
   * Test {@link ArchiveResource#isExists()}.
   * <p>
   * Method under test: {@link ArchiveResource#isExists()}
   */
  @Test
  public void testIsExists2() {
    // Arrange
    TarResource tarResource = new TarResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        null);

    // Act
    boolean actualIsExistsResult = tarResource.isExists();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getMode());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, tarResource.getLastModified());
    assertEquals(0L, tarResource.getSize());
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
    assertFalse(actualIsExistsResult);
  }

  /**
   * Test {@link ArchiveResource#isExists()}.
   * <p>
   * Method under test: {@link ArchiveResource#isExists()}
   */
  @Test
  public void testIsExists3() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.isExists());
  }

  /**
   * Test {@link ArchiveResource#isExists()}.
   * <p>
   * Method under test: {@link ArchiveResource#isExists()}
   */
  @Test
  public void testIsExists4() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.isExists());
  }

  /**
   * Test {@link ArchiveResource#isExists()}.
   * <p>
   * Method under test: {@link ArchiveResource#isExists()}
   */
  @Test
  public void testIsExists5() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.isExists());
  }

  /**
   * Test {@link ArchiveResource#isExists()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()} Name is {@code entry name not set}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#isExists()}
   */
  @Test
  public void testIsExists_givenTarResourceNameIsEntryNameNotSet_thenThrowBuildException() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.isExists());
  }

  /**
   * Test {@link ArchiveResource#isExists()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#isExists()}
   */
  @Test
  public void testIsExists_givenTarResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource()).isExists());
  }

  /**
   * Test {@link ArchiveResource#getMode()}.
   * <p>
   * Method under test: {@link ArchiveResource#getMode()}
   */
  @Test
  public void testGetMode() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("entry name not set"));

    // Act
    int actualMode = tarResource.getMode();

    // Assert
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, tarResource.getSize());
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
    assertEquals(33188, actualMode);
  }

  /**
   * Test {@link ArchiveResource#getMode()}.
   * <p>
   * Method under test: {@link ArchiveResource#getMode()}
   */
  @Test
  public void testGetMode2() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getMode());
  }

  /**
   * Test {@link ArchiveResource#getMode()}.
   * <p>
   * Method under test: {@link ArchiveResource#getMode()}
   */
  @Test
  public void testGetMode3() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getMode());
  }

  /**
   * Test {@link ArchiveResource#getMode()}.
   * <p>
   * Method under test: {@link ArchiveResource#getMode()}
   */
  @Test
  public void testGetMode4() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getMode());
  }

  /**
   * Test {@link ArchiveResource#getMode()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()} Name is {@code entry name not set}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#getMode()}
   */
  @Test
  public void testGetMode_givenTarResourceNameIsEntryNameNotSet_thenThrowBuildException() {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.getMode());
  }

  /**
   * Test {@link ArchiveResource#getMode()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#getMode()}
   */
  @Test
  public void testGetMode_givenTarResource_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource()).getMode());
  }

  /**
   * Test {@link ArchiveResource#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()}.</li>
   *   <li>Then {@link TarResource#TarResource()} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenTarResource_thenTarResourceReference() {
    // Arrange
    TarResource tarResource = new TarResource();
    Reference r = new Reference("42");

    // Act
    tarResource.setRefid(r);

    // Assert
    assertTrue(tarResource.isReference());
    assertSame(r, tarResource.getRefid());
  }

  /**
   * Test {@link ArchiveResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Given {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return minus thirty-eight.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_givenTarEntryWithName_thenReturnMinusThirtyEight() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("Name"));

    // Act and Assert
    assertEquals(-38,
        tarResource.compareTo(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link ArchiveResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return thirty-one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnThirtyOne() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("Name"));

    // Act and Assert
    assertEquals(31,
        tarResource.compareTo(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile())));
  }

  /**
   * Test {@link ArchiveResource#equals(Object)}, and {@link ArchiveResource#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ArchiveResource#equals(Object)}
   *   <li>{@link ArchiveResource#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("Name"));
    File a2 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource2 = new TarResource(a2, new TarEntry("Name"));

    // Act and Assert
    assertEquals(tarResource, tarResource2);
    int expectedHashCodeResult = tarResource.hashCode();
    assertEquals(expectedHashCodeResult, tarResource2.hashCode());
  }

  /**
   * Test {@link ArchiveResource#equals(Object)}, and {@link ArchiveResource#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ArchiveResource#equals(Object)}
   *   <li>{@link ArchiveResource#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    TarResource tarResource = new TarResource();

    // Act and Assert
    assertEquals(tarResource, tarResource);
    int expectedHashCodeResult = tarResource.hashCode();
    assertEquals(expectedHashCodeResult, tarResource.hashCode());
  }

  /**
   * Test {@link ArchiveResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("Name"));

    // Act and Assert
    assertNotEquals(tarResource, new TarResource());
  }

  /**
   * Test {@link ArchiveResource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("Name"));

    TarResource tarResource2 = new TarResource();
    tarResource2.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNotEquals(tarResource, tarResource2);
  }

  /**
   * Test {@link ArchiveResource#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new TarResource(), null);
  }

  /**
   * Test {@link ArchiveResource#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new TarResource(), "Different type to ArchiveResource");
  }

  /**
   * Test {@link ArchiveResource#toString()}.
   * <p>
   * Method under test: {@link ArchiveResource#toString()}
   */
  @Test
  public void testToString() {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    String actualToStringResult = (new TarResource(a, new TarEntry("Name"))).toString();

    // Assert
    assertEquals(String.join("", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), ":Name"),
        actualToStringResult);
  }

  /**
   * Test {@link ArchiveResource#toString()}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) addExpandProperties {@link ExpandProperties#ExpandProperties()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#toString()}
   */
  @Test
  public void testToString_givenFilterMapperAddExpandPropertiesExpandProperties() {
    // Arrange
    FilterMapper m = new FilterMapper();
    m.addExpandProperties(new ExpandProperties());
    MappedResource a = new MappedResource(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()), m);

    // Act and Assert
    assertEquals("test.txt:Name", (new TarResource(a, new TarEntry("Name"))).toString());
  }

  /**
   * Test {@link ArchiveResource#toString()}.
   * <ul>
   *   <li>Given {@link FilterMapper} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code test.txt:Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#toString()}
   */
  @Test
  public void testToString_givenFilterMapperProjectIsProject_thenReturnTestTxtName() {
    // Arrange
    FilterMapper m = new FilterMapper();
    m.setProject(new Project());
    m.addExpandProperties(new ExpandProperties());
    MappedResource a = new MappedResource(
        new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()), m);

    // Act and Assert
    assertEquals("test.txt:Name", (new TarResource(a, new TarEntry("Name"))).toString());
  }

  /**
   * Test {@link ArchiveResource#toString()}.
   * <ul>
   *   <li>Given {@link Resource#Resource(String)} with name is {@code (anonymous)}.</li>
   *   <li>Then return {@code (anonymous):Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#toString()}
   */
  @Test
  public void testToString_givenResourceWithNameIsAnonymous_thenReturnAnonymousName() {
    // Arrange
    Resource a = new Resource("(anonymous)");

    // Act and Assert
    assertEquals("(anonymous):Name", (new TarResource(a, new TarEntry("Name"))).toString());
  }

  /**
   * Test {@link ArchiveResource#toString()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource(Resource, TarEntry)} with a is {@link Resource#Resource()} and e is {@link TarEntry#TarEntry(String)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#toString()}
   */
  @Test
  public void testToString_givenTarResourceWithAIsResourceAndEIsTarEntry() {
    // Arrange
    Resource a = new Resource();

    // Act and Assert
    assertEquals("(anonymous):Name", (new TarResource(a, new TarEntry("Name"))).toString());
  }

  /**
   * Test {@link ArchiveResource#toString()}.
   * <ul>
   *   <li>Then return {@code null:Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#toString()}
   */
  @Test
  public void testToString_thenReturnNullName() {
    // Arrange
    FileResource r = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile());
    MappedResource a = new MappedResource(r, new FilterMapper());

    // Act and Assert
    assertEquals("null:Name", (new TarResource(a, new TarEntry("Name"))).toString());
  }

  /**
   * Test {@link ArchiveResource#toString()}.
   * <ul>
   *   <li>Then return {@code test.txt:Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#toString()}
   */
  @Test
  public void testToString_thenReturnTestTxtName() {
    // Arrange
    FileResource r = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    MappedResource a = new MappedResource(r, new FilterMapper());

    // Act and Assert
    assertEquals("test.txt:Name", (new TarResource(a, new TarEntry("Name"))).toString());
  }

  /**
   * Test {@link ArchiveResource#toString()}.
   * <ul>
   *   <li>Then return {@code (unbound file resource):Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#toString()}
   */
  @Test
  public void testToString_thenReturnUnboundFileResourceName() {
    // Arrange
    FileResource a = new FileResource();

    // Act and Assert
    assertEquals("(unbound file resource):Name", (new TarResource(a, new TarEntry("Name"))).toString());
  }

  /**
   * Test {@link ArchiveResource#checkEntry()}.
   * <p>
   * Method under test: {@link ArchiveResource#checkEntry()}
   */
  @Test
  public void testCheckEntry() throws BuildException {
    // Arrange
    File a = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    TarResource tarResource = new TarResource(a, new TarEntry("entry name not set"));

    // Act
    tarResource.checkEntry();

    // Assert that nothing has changed
    assertEquals("", tarResource.getGroup());
    assertEquals("", tarResource.getUserName());
    assertEquals(0, tarResource.getGid());
    assertEquals(0, tarResource.getUid());
    assertEquals(0L, tarResource.getSize());
    assertEquals(0L, tarResource.getLongGid());
    assertEquals(0L, tarResource.getLongUid());
    assertEquals(33188, tarResource.getMode());
  }

  /**
   * Test {@link ArchiveResource#checkEntry()}.
   * <p>
   * Method under test: {@link ArchiveResource#checkEntry()}
   */
  @Test
  public void testCheckEntry2() throws BuildException {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.checkEntry());
  }

  /**
   * Test {@link ArchiveResource#checkEntry()}.
   * <p>
   * Method under test: {@link ArchiveResource#checkEntry()}
   */
  @Test
  public void testCheckEntry3() throws BuildException {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.checkEntry());
  }

  /**
   * Test {@link ArchiveResource#checkEntry()}.
   * <p>
   * Method under test: {@link ArchiveResource#checkEntry()}
   */
  @Test
  public void testCheckEntry4() throws BuildException {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile());
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.checkEntry());
  }

  /**
   * Test {@link ArchiveResource#checkEntry()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()} Name is {@code entry name not set}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#checkEntry()}
   */
  @Test
  public void testCheckEntry_givenTarResourceNameIsEntryNameNotSet_thenThrowBuildException() throws BuildException {
    // Arrange
    TarResource tarResource = new TarResource();
    tarResource.setName("entry name not set");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarResource.checkEntry());
  }

  /**
   * Test {@link ArchiveResource#checkEntry()}.
   * <ul>
   *   <li>Given {@link TarResource#TarResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveResource#checkEntry()}
   */
  @Test
  public void testCheckEntry_givenTarResource_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TarResource()).checkEntry());
  }
}
