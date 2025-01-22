package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Stack;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.taskdefs.Tar;
import org.apache.tools.ant.taskdefs.Tar.TarFileSet;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.apache.tools.ant.types.resources.BZip2Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.Resources;
import org.apache.tools.ant.types.selectors.SelectSelector;
import org.junit.Test;

public class ArchiveFileSetDiffblueTest {
  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileList#FileList()}.</li>
   *   <li>When {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileList_whenConcatAddFilelistFileList() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    Concat a = new Concat();
    a.addFilelist(new FileList());

    // Act
    tarFileSet.addConfigured(a);

    // Assert
    assertFalse(tarFileSet.isFilesystemOnly());
    assertFalse(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code ..}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Src Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsDotDot_thenTarFileSetSrcNameIsDownloads() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    FileName name = new FileName();
    name.setName("..");

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act
    tarFileSet.addConfigured(a);

    // Assert
    File src = tarFileSet.getSrc();
    assertEquals("Downloads", src.getName());
    assertFalse(tarFileSet.isFilesystemOnly());
    assertFalse(tarFileSet.isChecked());
    assertTrue(src.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Src Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsDot_thenTarFileSetSrcNameIsApacheAnt11015() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    FileName name = new FileName();
    name.setName(".");

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act
    tarFileSet.addConfigured(a);

    // Assert
    File src = tarFileSet.getSrc();
    assertEquals("apache-ant-1.10.15", src.getName());
    assertFalse(tarFileSet.isFilesystemOnly());
    assertFalse(tarFileSet.isChecked());
    assertTrue(src.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsEmptyString() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    FileName name = new FileName();
    name.setName("");

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act
    tarFileSet.addConfigured(a);

    // Assert
    File src = tarFileSet.getSrc();
    assertEquals("apache-ant-1.10.15", src.getName());
    assertFalse(tarFileSet.isFilesystemOnly());
    assertFalse(tarFileSet.isChecked());
    assertTrue(src.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Src Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsName_thenTarFileSetSrcNameIsName() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    FileName name = new FileName();
    name.setName("Name");

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act
    tarFileSet.addConfigured(a);

    // Assert
    File src = tarFileSet.getSrc();
    assertEquals("Name", src.getName());
    assertFalse(tarFileSet.isFilesystemOnly());
    assertFalse(tarFileSet.isChecked());
    assertTrue(src.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FilterChain} (default constructor).</li>
   *   <li>When {@link Concat} (default constructor) addFilterChain {@link FilterChain} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFilterChain_whenConcatAddFilterChainFilterChain() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    Concat a = new Concat();
    a.addFilterChain(new FilterChain());
    a.addText("Text");

    // Act
    tarFileSet.addConfigured(a);

    // Assert
    assertFalse(tarFileSet.isFilesystemOnly());
    assertFalse(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenResource_whenConcatDestIsResource() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    Concat a = new Concat();
    a.setDest(new Resource());
    a.addFilelist(new FileList());

    // Act
    tarFileSet.addConfigured(a);

    // Assert
    assertFalse(tarFileSet.isFilesystemOnly());
    assertFalse(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>When {@link FileList#FileList()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenTarFileSet_whenFileList_thenThrowBuildException() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act and Assert
    assertThrows(BuildException.class, () -> tarFileSet.addConfigured(new FileList()));
  }

  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenTarFileSet_whenNone_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new TarFileSet()).addConfigured(Resources.NONE));
  }

  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@code Text}.</li>
   *   <li>When {@link Concat} (default constructor) addText {@code Text}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenText_whenConcatAddTextText() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    Concat a = new Concat();
    a.addText("Text");

    // Act
    tarFileSet.addConfigured(a);

    // Assert
    assertFalse(tarFileSet.isFilesystemOnly());
    assertFalse(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Src Name is {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenPathWithPIsProjectAndPath_thenTarFileSetSrcNameIsPath() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.addConfigured(new Path(new Project(), "Path"));

    // Assert
    File src = tarFileSet.getSrc();
    assertEquals("Path", src.getName());
    assertFalse(tarFileSet.isFilesystemOnly());
    assertFalse(tarFileSet.isChecked());
    assertTrue(src.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then not {@link Tar.TarFileSet#TarFileSet()} FilesystemOnly.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenTarFileSet_thenNotTarFileSetFilesystemOnly() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setSrcResource(new Resource());

    // Assert
    assertFalse(tarFileSet.isFilesystemOnly());
    assertFalse(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#setSrcResource(Resource)}.
   * <ul>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} DirectoryScanner {@link TarScanner}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_thenTarFileSetDirectoryScannerTarScanner() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setProject(new Project());

    // Act
    tarFileSet.setSrcResource(new Resource());

    // Assert
    DirectoryScanner directoryScanner = tarFileSet.getDirectoryScanner();
    assertTrue(directoryScanner instanceof TarScanner);
    File dir = tarFileSet.getDir();
    assertEquals("apache-ant-1.10.15", dir.getName());
    assertNull(((TarScanner) directoryScanner).srcFile);
    assertEquals(0, directoryScanner.getNotFollowedSymlinks().length);
    assertTrue(dir.isAbsolute());
    assertTrue(directoryScanner.isCaseSensitive());
    assertTrue(directoryScanner.isEverythingIncluded());
    assertTrue(directoryScanner.isFollowSymlinks());
    assertSame(dir, directoryScanner.getBasedir());
    assertSame(dir, tarFileSet.getProject().getBaseDir());
  }

  /**
   * Test {@link ArchiveFileSet#setSrcResource(Resource)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_thenThrowBuildException() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> tarFileSet.setSrcResource(new Resource()));
  }

  /**
   * Test {@link ArchiveFileSet#getSrc(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then return Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc(Project)}
   */
  @Test
  public void testGetSrcWithProject_givenFileResourceNameIsDotDot_thenReturnNameIsDownloads() {
    // Arrange
    FileResource src = new FileResource();
    src.setName("..");

    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(src);

    // Act
    File actualSrc = tarFileSet.getSrc(new Project());

    // Assert
    assertEquals("Downloads", actualSrc.getName());
    assertTrue(actualSrc.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc(Project)}
   */
  @Test
  public void testGetSrcWithProject_givenFileResourceNameIsDot_thenReturnNameIsApacheAnt11015() {
    // Arrange
    FileResource src = new FileResource();
    src.setName(".");

    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(src);

    // Act
    File actualSrc = tarFileSet.getSrc(new Project());

    // Assert
    assertEquals("apache-ant-1.10.15", actualSrc.getName());
    assertTrue(actualSrc.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc(Project)}
   */
  @Test
  public void testGetSrcWithProject_givenFileResourceNameIsEmptyString() {
    // Arrange
    FileResource src = new FileResource();
    src.setName("");

    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(src);

    // Act
    File actualSrc = tarFileSet.getSrc(new Project());

    // Assert
    assertEquals("apache-ant-1.10.15", actualSrc.getName());
    assertTrue(actualSrc.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code Name}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc(Project)}
   */
  @Test
  public void testGetSrcWithProject_givenFileResourceNameIsName_thenReturnName() {
    // Arrange
    FileResource src = new FileResource();
    src.setName("Name");

    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(src);

    // Act
    File actualSrc = tarFileSet.getSrc(new Project());

    // Assert
    assertEquals("Name", actualSrc.getName());
    assertTrue(actualSrc.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc(Project)}
   */
  @Test
  public void testGetSrcWithProject_givenTarFileSetSrcResourceIsBZip2Resource_thenReturnNull() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new BZip2Resource());

    // Act and Assert
    assertNull(tarFileSet.getSrc(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#getSrc(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc(Project)}
   */
  @Test
  public void testGetSrcWithProject_givenTarFileSetSrcResourceIsFileResource_thenReturnNull() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new FileResource());

    // Act and Assert
    assertNull(tarFileSet.getSrc(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#getSrc(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc(Project)}
   */
  @Test
  public void testGetSrcWithProject_givenTarFileSetSrcResourceIsNull_thenReturnNull() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(null);

    // Act and Assert
    assertNull(tarFileSet.getSrc(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#getSrc(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@link Resource#Resource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc(Project)}
   */
  @Test
  public void testGetSrcWithProject_givenTarFileSetSrcResourceIsResource_thenReturnNull() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new Resource());

    // Act and Assert
    assertNull(tarFileSet.getSrc(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#getSrc(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc(Project)}
   */
  @Test
  public void testGetSrcWithProject_givenTarFileSet_thenReturnNull() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act and Assert
    assertNull(tarFileSet.getSrc(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#getSrc(Project)} with {@code Project}.
   * <ul>
   *   <li>Then return Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc(Project)}
   */
  @Test
  public void testGetSrcWithProject_thenReturnNameIsTestTxt() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));

    // Act
    File actualSrc = tarFileSet.getSrc(new Project());

    // Assert
    assertEquals("test.txt", actualSrc.getName());
    assertTrue(actualSrc.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then return Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc()}
   */
  @Test
  public void testGetSrc_givenFileResourceNameIsDotDot_thenReturnNameIsDownloads() {
    // Arrange
    FileResource src = new FileResource();
    src.setName("..");

    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(src);

    // Act
    File actualSrc = tarFileSet.getSrc();

    // Assert
    assertEquals("Downloads", actualSrc.getName());
    assertTrue(actualSrc.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc()}
   */
  @Test
  public void testGetSrc_givenFileResourceNameIsDot_thenReturnNameIsApacheAnt11015() {
    // Arrange
    FileResource src = new FileResource();
    src.setName(".");

    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(src);

    // Act
    File actualSrc = tarFileSet.getSrc();

    // Assert
    assertEquals("apache-ant-1.10.15", actualSrc.getName());
    assertTrue(actualSrc.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is empty string.</li>
   *   <li>Then return Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc()}
   */
  @Test
  public void testGetSrc_givenFileResourceNameIsEmptyString_thenReturnNameIsApacheAnt11015() {
    // Arrange
    FileResource src = new FileResource();
    src.setName("");

    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(src);

    // Act
    File actualSrc = tarFileSet.getSrc();

    // Assert
    assertEquals("apache-ant-1.10.15", actualSrc.getName());
    assertTrue(actualSrc.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code Name}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc()}
   */
  @Test
  public void testGetSrc_givenFileResourceNameIsName_thenReturnName() {
    // Arrange
    FileResource src = new FileResource();
    src.setName("Name");

    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(src);

    // Act
    File actualSrc = tarFileSet.getSrc();

    // Assert
    assertEquals("Name", actualSrc.getName());
    assertTrue(actualSrc.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc()}
   */
  @Test
  public void testGetSrc_givenTarFileSetSrcResourceIsBZip2Resource_thenReturnNull() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new BZip2Resource());

    // Act and Assert
    assertNull(tarFileSet.getSrc());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc()}
   */
  @Test
  public void testGetSrc_givenTarFileSetSrcResourceIsFileResource_thenReturnNull() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new FileResource());

    // Act and Assert
    assertNull(tarFileSet.getSrc());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc()}
   */
  @Test
  public void testGetSrc_givenTarFileSetSrcResourceIsNull_thenReturnNull() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(null);

    // Act and Assert
    assertNull(tarFileSet.getSrc());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@link Resource#Resource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc()}
   */
  @Test
  public void testGetSrc_givenTarFileSetSrcResourceIsResource_thenReturnNull() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new Resource());

    // Act and Assert
    assertNull(tarFileSet.getSrc());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc()}
   */
  @Test
  public void testGetSrc_givenTarFileSet_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new TarFileSet()).getSrc());
  }

  /**
   * Test {@link ArchiveFileSet#getSrc()}.
   * <ul>
   *   <li>Then return Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getSrc()}
   */
  @Test
  public void testGetSrc_thenReturnNameIsTestTxt() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));

    // Act
    File actualSrc = tarFileSet.getSrc();

    // Assert
    assertEquals("test.txt", actualSrc.getName());
    assertTrue(actualSrc.isAbsolute());
  }

  /**
   * Test {@link ArchiveFileSet#setPrefix(String)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} Fullpath is {@code Fullpath}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setPrefix(String)}
   */
  @Test
  public void testSetPrefix_givenTarFileSetFullpathIsFullpath_thenThrowBuildException() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setFullpath("Fullpath");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarFileSet.setPrefix("Prefix"));
  }

  /**
   * Test {@link ArchiveFileSet#setPrefix(String)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Prefix is {@code Prefix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setPrefix(String)}
   */
  @Test
  public void testSetPrefix_givenTarFileSetProjectIsProject_thenTarFileSetPrefixIsPrefix() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setProject(new Project());

    // Act
    tarFileSet.setPrefix("Prefix");

    // Assert
    assertEquals("Prefix", tarFileSet.getPrefix());
  }

  /**
   * Test {@link ArchiveFileSet#setPrefix(String)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>When empty string.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Prefix is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setPrefix(String)}
   */
  @Test
  public void testSetPrefix_givenTarFileSet_whenEmptyString_thenTarFileSetPrefixIsEmptyString() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setPrefix("");

    // Assert that nothing has changed
    assertEquals("", tarFileSet.getPrefix());
  }

  /**
   * Test {@link ArchiveFileSet#setPrefix(String)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>When {@code Prefix}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Prefix is {@code Prefix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setPrefix(String)}
   */
  @Test
  public void testSetPrefix_givenTarFileSet_whenPrefix_thenTarFileSetPrefixIsPrefix() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setPrefix("Prefix");

    // Assert
    assertEquals("Prefix", tarFileSet.getPrefix());
  }

  /**
   * Test {@link ArchiveFileSet#getPrefix()}.
   * <p>
   * Method under test: {@link ArchiveFileSet#getPrefix()}
   */
  @Test
  public void testGetPrefix() {
    // Arrange, Act and Assert
    assertEquals("", (new TarFileSet()).getPrefix());
  }

  /**
   * Test {@link ArchiveFileSet#getPrefix(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getPrefix(Project)}
   */
  @Test
  public void testGetPrefixWithProject_givenTarFileSet() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act and Assert
    assertEquals("", tarFileSet.getPrefix(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#getPrefix(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getPrefix(Project)}
   */
  @Test
  public void testGetPrefixWithProject_givenTarFileSetAppendSelectorScriptSelector() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("", tarFileSet.getPrefix(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#setFullpath(String)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} Prefix is {@code Prefix}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setFullpath(String)}
   */
  @Test
  public void testSetFullpath_givenTarFileSetPrefixIsPrefix_thenThrowBuildException() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setPrefix("Prefix");

    // Act and Assert
    assertThrows(BuildException.class, () -> tarFileSet.setFullpath("Fullpath"));
  }

  /**
   * Test {@link ArchiveFileSet#setFullpath(String)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Fullpath is {@code Fullpath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setFullpath(String)}
   */
  @Test
  public void testSetFullpath_givenTarFileSetProjectIsProject_thenTarFileSetFullpathIsFullpath() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setProject(new Project());

    // Act
    tarFileSet.setFullpath("Fullpath");

    // Assert
    assertEquals("Fullpath", tarFileSet.getFullpath());
  }

  /**
   * Test {@link ArchiveFileSet#setFullpath(String)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>When {@code Fullpath}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Fullpath is {@code Fullpath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setFullpath(String)}
   */
  @Test
  public void testSetFullpath_givenTarFileSet_whenFullpath_thenTarFileSetFullpathIsFullpath() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setFullpath("Fullpath");

    // Assert
    assertEquals("Fullpath", tarFileSet.getFullpath());
  }

  /**
   * Test {@link ArchiveFileSet#setFullpath(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Fullpath is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setFullpath(String)}
   */
  @Test
  public void testSetFullpath_whenEmptyString_thenTarFileSetFullpathIsEmptyString() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setPrefix("Prefix");

    // Act
    tarFileSet.setFullpath("");

    // Assert that nothing has changed
    assertEquals("", tarFileSet.getFullpath());
  }

  /**
   * Test {@link ArchiveFileSet#getFullpath()}.
   * <p>
   * Method under test: {@link ArchiveFileSet#getFullpath()}
   */
  @Test
  public void testGetFullpath() {
    // Arrange, Act and Assert
    assertEquals("", (new TarFileSet()).getFullpath());
  }

  /**
   * Test {@link ArchiveFileSet#getFullpath(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getFullpath(Project)}
   */
  @Test
  public void testGetFullpathWithProject_givenTarFileSet() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act and Assert
    assertEquals("", tarFileSet.getFullpath(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#getFullpath(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getFullpath(Project)}
   */
  @Test
  public void testGetFullpathWithProject_givenTarFileSetAppendSelectorScriptSelector() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals("", tarFileSet.getFullpath(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#setEncoding(String)}.
   * <p>
   * Method under test: {@link ArchiveFileSet#setEncoding(String)}
   */
  @Test
  public void testSetEncoding() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setEncoding("Enc");

    // Assert
    assertEquals("Enc", tarFileSet.getEncoding());
  }

  /**
   * Test {@link ArchiveFileSet#getEncoding()}.
   * <p>
   * Method under test: {@link ArchiveFileSet#getEncoding()}
   */
  @Test
  public void testGetEncoding() {
    // Arrange, Act and Assert
    assertNull((new TarFileSet()).getEncoding());
  }

  /**
   * Test {@link ArchiveFileSet#iterator()}.
   * <p>
   * Method under test: {@link ArchiveFileSet#iterator()}
   */
  @Test
  public void testIterator() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    tarFileSet.appendSelector(new ScriptSelector());

    // Act and Assert
    assertThrows(BuildException.class, () -> tarFileSet.iterator());
  }

  /**
   * Test {@link ArchiveFileSet#iterator()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@link Resource#Resource(String)} with {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#iterator()}
   */
  @Test
  public void testIterator_givenTarFileSetSrcResourceIsResourceWithName() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new Resource("Name"));
    tarFileSet.appendSelector(new ScriptSelector());

    // Act and Assert
    assertThrows(BuildException.class, () -> tarFileSet.iterator());
  }

  /**
   * Test {@link ArchiveFileSet#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenTarFileSetSrcResourceIsNull_thenReturnTrue() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(null);

    // Act and Assert
    assertTrue(tarFileSet.isFilesystemOnly());
  }

  /**
   * Test {@link ArchiveFileSet#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenTarFileSetSrcResourceIsResource_thenReturnFalse() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new Resource());

    // Act and Assert
    assertFalse(tarFileSet.isFilesystemOnly());
  }

  /**
   * Test {@link ArchiveFileSet#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenTarFileSet_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new TarFileSet()).isFilesystemOnly());
  }

  /**
   * Test {@link ArchiveFileSet#setFileMode(String)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Mode is {@code 32802}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setFileMode(String)}
   */
  @Test
  public void testSetFileMode_givenTarFileSet_when42_thenTarFileSetModeIs32802() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setFileMode("42");

    // Assert
    assertEquals(32802, tarFileSet.getMode());
    assertEquals(32802, tarFileSet.getFileMode());
  }

  /**
   * Test {@link ArchiveFileSet#integerSetFileMode(int)}.
   * <p>
   * Method under test: {@link ArchiveFileSet#integerSetFileMode(int)}
   */
  @Test
  public void testIntegerSetFileMode() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.integerSetFileMode(1);

    // Assert
    assertEquals(32769, tarFileSet.getMode());
    assertEquals(32769, tarFileSet.getFileMode());
  }

  /**
   * Test {@link ArchiveFileSet#getFileMode()}.
   * <p>
   * Method under test: {@link ArchiveFileSet#getFileMode()}
   */
  @Test
  public void testGetFileMode() {
    // Arrange, Act and Assert
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, (new TarFileSet()).getFileMode());
  }

  /**
   * Test {@link ArchiveFileSet#getFileMode(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getFileMode(Project)}
   */
  @Test
  public void testGetFileModeWithProject_givenTarFileSet() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act and Assert
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, tarFileSet.getFileMode(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#getFileMode(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getFileMode(Project)}
   */
  @Test
  public void testGetFileModeWithProject_givenTarFileSetAppendSelectorScriptSelector() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, tarFileSet.getFileMode(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#hasFileModeBeenSet()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#hasFileModeBeenSet()}
   */
  @Test
  public void testHasFileModeBeenSet_givenTarFileSet() {
    // Arrange, Act and Assert
    assertFalse((new TarFileSet()).hasFileModeBeenSet());
  }

  /**
   * Test {@link ArchiveFileSet#hasFileModeBeenSet()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#hasFileModeBeenSet()}
   */
  @Test
  public void testHasFileModeBeenSet_givenTarFileSetAppendSelectorScriptSelector() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.appendSelector(new ScriptSelector());

    // Act and Assert
    assertFalse(tarFileSet.hasFileModeBeenSet());
  }

  /**
   * Test {@link ArchiveFileSet#setDirMode(String)}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} DirMode is {@code 16418}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#setDirMode(String)}
   */
  @Test
  public void testSetDirMode_givenTarFileSet_when42_thenTarFileSetDirModeIs16418() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.setDirMode("42");

    // Assert
    assertEquals(16418, tarFileSet.getDirMode());
  }

  /**
   * Test {@link ArchiveFileSet#integerSetDirMode(int)}.
   * <p>
   * Method under test: {@link ArchiveFileSet#integerSetDirMode(int)}
   */
  @Test
  public void testIntegerSetDirMode() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act
    tarFileSet.integerSetDirMode(1);

    // Assert
    assertEquals(16385, tarFileSet.getDirMode());
  }

  /**
   * Test {@link ArchiveFileSet#getDirMode()}.
   * <p>
   * Method under test: {@link ArchiveFileSet#getDirMode()}
   */
  @Test
  public void testGetDirMode() {
    // Arrange, Act and Assert
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, (new TarFileSet()).getDirMode());
  }

  /**
   * Test {@link ArchiveFileSet#getDirMode(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getDirMode(Project)}
   */
  @Test
  public void testGetDirModeWithProject_givenTarFileSet() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    // Act and Assert
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, tarFileSet.getDirMode(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#getDirMode(Project)} with {@code Project}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#getDirMode(Project)}
   */
  @Test
  public void testGetDirModeWithProject_givenTarFileSetAppendSelectorScriptSelector() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.appendSelector(new ScriptSelector());

    // Act and Assert
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, tarFileSet.getDirMode(new Project()));
  }

  /**
   * Test {@link ArchiveFileSet#hasDirModeBeenSet()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#hasDirModeBeenSet()}
   */
  @Test
  public void testHasDirModeBeenSet_givenTarFileSet() {
    // Arrange, Act and Assert
    assertFalse((new TarFileSet()).hasDirModeBeenSet());
  }

  /**
   * Test {@link ArchiveFileSet#hasDirModeBeenSet()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#hasDirModeBeenSet()}
   */
  @Test
  public void testHasDirModeBeenSet_givenTarFileSetAppendSelectorScriptSelector() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.appendSelector(new ScriptSelector());

    // Act and Assert
    assertFalse(tarFileSet.hasDirModeBeenSet());
  }

  /**
   * Test {@link ArchiveFileSet#configureFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@code Fullpath}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#configureFileSet(ArchiveFileSet)}
   */
  @Test
  public void testConfigureFileSet_givenFullpath_thenThrowBuildException() {
    // Arrange
    ZipFileSet zipFileSet = new ZipFileSet();
    zipFileSet.setPrefix("Prefix");

    TarFileSet zfs = new TarFileSet();
    zfs.setFullpath("Fullpath");

    // Act and Assert
    assertThrows(BuildException.class, () -> zipFileSet.configureFileSet(zfs));
  }

  /**
   * Test {@link ArchiveFileSet#configureFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link Tar.TarFileSet#TarFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#configureFileSet(ArchiveFileSet)}
   */
  @Test
  public void testConfigureFileSet_givenProject_whenTarFileSetProjectIsProject() {
    // Arrange
    ZipFileSet zipFileSet = new ZipFileSet();

    TarFileSet zfs = new TarFileSet();
    zfs.setProject(new Project());

    // Act
    zipFileSet.configureFileSet(zfs);

    // Assert that nothing has changed
    assertEquals("", zfs.getPrefix());
  }

  /**
   * Test {@link ArchiveFileSet#configureFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>When {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Prefix is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#configureFileSet(ArchiveFileSet)}
   */
  @Test
  public void testConfigureFileSet_whenTarFileSet_thenTarFileSetPrefixIsEmptyString() {
    // Arrange
    ZipFileSet zipFileSet = new ZipFileSet();
    TarFileSet zfs = new TarFileSet();

    // Act
    zipFileSet.configureFileSet(zfs);

    // Assert that nothing has changed
    assertEquals("", zfs.getPrefix());
  }

  /**
   * Test {@link ArchiveFileSet#configureFileSet(ArchiveFileSet)}.
   * <ul>
   *   <li>When {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Prefix is {@code Prefix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#configureFileSet(ArchiveFileSet)}
   */
  @Test
  public void testConfigureFileSet_whenTarFileSet_thenTarFileSetPrefixIsPrefix() {
    // Arrange
    ZipFileSet zipFileSet = new ZipFileSet();
    zipFileSet.setPrefix("Prefix");
    TarFileSet zfs = new TarFileSet();

    // Act
    zipFileSet.configureFileSet(zfs);

    // Assert
    assertEquals("Prefix", zfs.getPrefix());
  }

  /**
   * Test {@link ArchiveFileSet#clone()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return not Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#clone()}
   */
  @Test
  public void testClone_givenTarFileSetAppendSelectorScriptSelector_thenReturnNotChecked() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.appendSelector(new ScriptSelector());

    // Act
    Object actualCloneResult = tarFileSet.clone();

    // Assert
    assertTrue(actualCloneResult instanceof TarFileSet);
    assertEquals("", ((TarFileSet) actualCloneResult).getFullpath());
    assertEquals("", ((TarFileSet) actualCloneResult).getPrefix());
    assertEquals("", ((TarFileSet) actualCloneResult).getGroup());
    assertEquals("", ((TarFileSet) actualCloneResult).getUserName());
    assertEquals("Tar$TarFileSet", ((TarFileSet) actualCloneResult).getDataTypeName());
    assertNull(((TarFileSet) actualCloneResult).getDir());
    assertNull(((TarFileSet) actualCloneResult).getSrc());
    assertNull(((TarFileSet) actualCloneResult).getDescription());
    assertNull(((TarFileSet) actualCloneResult).getEncoding());
    assertNull(((TarFileSet) actualCloneResult).getProject());
    assertNull(((TarFileSet) actualCloneResult).getRefid());
    assertEquals(0, ((TarFileSet) actualCloneResult).getGid());
    assertEquals(0, ((TarFileSet) actualCloneResult).getUid());
    assertEquals(5, ((TarFileSet) actualCloneResult).getMaxLevelsOfSymlinks());
    assertFalse(((TarFileSet) actualCloneResult).getPreserveLeadingSlashes());
    assertFalse(((TarFileSet) actualCloneResult).isChecked());
    assertFalse(((TarFileSet) actualCloneResult).isReference());
    assertFalse(((TarFileSet) actualCloneResult).hasGroupBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasGroupIdBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasUserIdBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasUserNameBeenSet());
    assertTrue(((TarFileSet) actualCloneResult).getDefaultexcludes());
    assertTrue(((TarFileSet) actualCloneResult).getErrorOnMissingDir());
    assertTrue(((TarFileSet) actualCloneResult).isFilesystemOnly());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, ((TarFileSet) actualCloneResult).getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, ((TarFileSet) actualCloneResult).getMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, ((TarFileSet) actualCloneResult).getFileMode());
  }

  /**
   * Test {@link ArchiveFileSet#clone()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then return Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#clone()}
   */
  @Test
  public void testClone_givenTarFileSet_thenReturnChecked() {
    // Arrange and Act
    Object actualCloneResult = (new TarFileSet()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof TarFileSet);
    assertEquals("", ((TarFileSet) actualCloneResult).getFullpath());
    assertEquals("", ((TarFileSet) actualCloneResult).getPrefix());
    assertEquals("", ((TarFileSet) actualCloneResult).getGroup());
    assertEquals("", ((TarFileSet) actualCloneResult).getUserName());
    assertEquals("Tar$TarFileSet", ((TarFileSet) actualCloneResult).getDataTypeName());
    assertNull(((TarFileSet) actualCloneResult).getDir());
    assertNull(((TarFileSet) actualCloneResult).getSrc());
    assertNull(((TarFileSet) actualCloneResult).getDescription());
    assertNull(((TarFileSet) actualCloneResult).getEncoding());
    assertNull(((TarFileSet) actualCloneResult).getProject());
    assertNull(((TarFileSet) actualCloneResult).getRefid());
    assertEquals(0, ((TarFileSet) actualCloneResult).getGid());
    assertEquals(0, ((TarFileSet) actualCloneResult).getUid());
    assertEquals(5, ((TarFileSet) actualCloneResult).getMaxLevelsOfSymlinks());
    assertFalse(((TarFileSet) actualCloneResult).getPreserveLeadingSlashes());
    assertFalse(((TarFileSet) actualCloneResult).isReference());
    assertFalse(((TarFileSet) actualCloneResult).hasGroupBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasGroupIdBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasUserIdBeenSet());
    assertFalse(((TarFileSet) actualCloneResult).hasUserNameBeenSet());
    assertTrue(((TarFileSet) actualCloneResult).getDefaultexcludes());
    assertTrue(((TarFileSet) actualCloneResult).getErrorOnMissingDir());
    assertTrue(((TarFileSet) actualCloneResult).isFilesystemOnly());
    assertTrue(((TarFileSet) actualCloneResult).isChecked());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, ((TarFileSet) actualCloneResult).getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, ((TarFileSet) actualCloneResult).getMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, ((TarFileSet) actualCloneResult).getFileMode());
  }

  /**
   * Test {@link ArchiveFileSet#toString()}.
   * <p>
   * Method under test: {@link ArchiveFileSet#toString()}
   */
  @Test
  public void testToString() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertNull(tarFileSet.toString());
  }

  /**
   * Test {@link ArchiveFileSet#toString()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@link Resource#Resource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#toString()}
   */
  @Test
  public void testToString_givenTarFileSetSrcResourceIsResource_thenReturnNull() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new Resource());

    // Act and Assert
    assertNull(tarFileSet.toString());
  }

  /**
   * Test {@link ArchiveFileSet#toString()}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#toString()}
   */
  @Test
  public void testToString_givenTarFileSet_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new TarFileSet()).toString());
  }

  /**
   * Test {@link ArchiveFileSet#toString()}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#toString()}
   */
  @Test
  public void testToString_thenReturnEmptyString() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile()));

    // Act and Assert
    assertEquals("", tarFileSet.toString());
  }

  /**
   * Test {@link ArchiveFileSet#toString()}.
   * <ul>
   *   <li>Then return {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#toString()}
   */
  @Test
  public void testToString_thenReturnFileAttributeIsNull() {
    // Arrange
    FileResource src = new FileResource();
    src.setName("file attribute is null!");

    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(src);

    // Act and Assert
    assertEquals("file attribute is null!", tarFileSet.toString());
  }

  /**
   * Test {@link ArchiveFileSet#toString()}.
   * <ul>
   *   <li>Then return {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#toString()}
   */
  @Test
  public void testToString_thenReturnTestTxt() {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));

    // Act and Assert
    assertEquals("test.txt", tarFileSet.toString());
  }

  /**
   * Test {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_given42_whenStackAdd42_thenTarFileSetChecked() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    Stack<Object> stk = new Stack<>();
    stk.add("42");

    // Act
    tarFileSet.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_given42_whenStackAdd42_thenTarFileSetChecked2() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();

    Stack<Object> stk = new Stack<>();
    stk.add("42");
    stk.add("42");

    // Act
    tarFileSet.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} addSelector {@link SelectSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenTarFileSetAddSelectorSelectSelector() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.addSelector(new SelectSelector());
    Stack<Object> stk = new Stack<>();

    // Act
    tarFileSet.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenTarFileSetAppendSelectorScriptSelector() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.appendSelector(new ScriptSelector());
    Stack<Object> stk = new Stack<>();

    // Act
    tarFileSet.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenTarFileSetAppendSelectorScriptSelector2() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.appendSelector(new ScriptSelector());
    tarFileSet.appendSelector(new ScriptSelector());
    Stack<Object> stk = new Stack<>();

    // Act
    tarFileSet.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} Checked is {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenTarFileSetCheckedIsFalse() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setChecked(false);
    Stack<Object> stk = new Stack<>();

    // Act
    tarFileSet.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()} SrcResource is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenTarFileSetSrcResourceIsResource() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    tarFileSet.setSrcResource(new Resource());
    Stack<Object> stk = new Stack<>();

    // Act
    tarFileSet.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(tarFileSet.isChecked());
  }

  /**
   * Test {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link Tar.TarFileSet#TarFileSet()}.</li>
   *   <li>Then {@link Tar.TarFileSet#TarFileSet()} Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveFileSet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenTarFileSet_thenTarFileSetChecked() throws BuildException {
    // Arrange
    TarFileSet tarFileSet = new TarFileSet();
    Stack<Object> stk = new Stack<>();

    // Act
    tarFileSet.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(tarFileSet.isChecked());
  }
}
