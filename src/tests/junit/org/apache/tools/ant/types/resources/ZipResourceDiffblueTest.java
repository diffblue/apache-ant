package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.zip.AsiExtraField;
import org.apache.tools.zip.ZipEntry;
import org.apache.tools.zip.ZipExtraField;
import org.junit.Test;

public class ZipResourceDiffblueTest {
  /**
   * Test {@link ZipResource#ZipResource(File, String, ZipEntry)}.
   * <ul>
   *   <li>Given {@link AsiExtraField} (default constructor).</li>
   *   <li>Then first element return {@link AsiExtraField}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#ZipResource(File, String, ZipEntry)}
   */
  @Test
  public void testNewZipResource_givenAsiExtraField_thenFirstElementReturnAsiExtraField() {
    // Arrange
    File z = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry e = new ZipEntry("Name");
    e.addExtraField(new AsiExtraField());

    // Act and Assert
    ZipExtraField[] extraFields = (new ZipResource(z, "Enc", e)).getExtraFields();
    ZipExtraField zipExtraField = extraFields[0];
    assertTrue(zipExtraField instanceof AsiExtraField);
    assertEquals(1, extraFields.length);
    assertArrayEquals(new byte[]{14, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link ZipResource#ZipResource(File, String, ZipEntry)}.
   * <ul>
   *   <li>Given one.</li>
   *   <li>Then return Mode is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#ZipResource(File, String, ZipEntry)}
   */
  @Test
  public void testNewZipResource_givenOne_thenReturnModeIsOne() {
    // Arrange
    File z = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry e = new ZipEntry("Name");
    e.setUnixMode(1);
    e.addExtraField(new AsiExtraField());

    // Act
    ZipResource actualZipResource = new ZipResource(z, "Enc", e);

    // Assert
    ZipExtraField[] extraFields = actualZipResource.getExtraFields();
    ZipExtraField zipExtraField = extraFields[0];
    assertTrue(zipExtraField instanceof AsiExtraField);
    assertEquals(1, actualZipResource.getMode());
    assertEquals(1, extraFields.length);
    assertArrayEquals(new byte[]{14, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link ZipResource#ZipResource(File, String, ZipEntry)}.
   * <ul>
   *   <li>Given three.</li>
   *   <li>Then return Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#ZipResource(File, String, ZipEntry)}
   */
  @Test
  public void testNewZipResource_givenThree_thenReturnSizeIsThree() {
    // Arrange
    File z = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    ZipEntry e = new ZipEntry("Name");
    e.setSize(3L);
    e.addExtraField(new AsiExtraField());

    // Act
    ZipResource actualZipResource = new ZipResource(z, "Enc", e);

    // Assert
    ZipExtraField[] extraFields = actualZipResource.getExtraFields();
    ZipExtraField zipExtraField = extraFields[0];
    assertTrue(zipExtraField instanceof AsiExtraField);
    assertEquals(1, extraFields.length);
    assertEquals(3L, actualZipResource.getSize());
    assertArrayEquals(new byte[]{14, 0}, zipExtraField.getCentralDirectoryLength().getBytes());
    assertArrayEquals(new byte[]{'n', 'u'}, zipExtraField.getHeaderId().getBytes());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{'v', 'h', -118, -29, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
        zipExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link ZipResource#getZipfile()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code Name}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#getZipfile()}
   */
  @Test
  public void testGetZipfile_givenFileResourceNameIsName_thenReturnName() {
    // Arrange
    FileResource a = new FileResource();
    a.setName("Name");

    ZipResource zipResource = new ZipResource();
    zipResource.addConfigured(a);

    // Act
    File actualZipfile = zipResource.getZipfile();

    // Assert
    assertEquals("Name", actualZipfile.getName());
    assertTrue(actualZipfile.isAbsolute());
  }

  /**
   * Test {@link ZipResource#getZipfile()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry(String)} with {@code Name}.</li>
   *   <li>Then return Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#getZipfile()}
   */
  @Test
  public void testGetZipfile_givenZipEntryWithName_thenReturnNameIsTestTxt() {
    // Arrange
    File z = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act
    File actualZipfile = (new ZipResource(z, "Enc", new ZipEntry("Name"))).getZipfile();

    // Assert
    assertEquals("test.txt", actualZipfile.getName());
    assertTrue(actualZipfile.isAbsolute());
  }

  /**
   * Test {@link ZipResource#getZipfile()}.
   * <ul>
   *   <li>Given {@link ZipResource#ZipResource()} addConfigured {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#getZipfile()}
   */
  @Test
  public void testGetZipfile_givenZipResourceAddConfiguredFileResource_thenReturnNull() {
    // Arrange
    ZipResource zipResource = new ZipResource();
    zipResource.addConfigured(new FileResource());

    // Act and Assert
    assertNull(zipResource.getZipfile());
  }

  /**
   * Test {@link ZipResource#getZipfile()}.
   * <ul>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#getZipfile()}
   */
  @Test
  public void testGetZipfile_thenReturnName() {
    // Arrange
    FileResource a = new FileResource();
    a.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    a.setName("Name");

    ZipResource zipResource = new ZipResource();
    zipResource.addConfigured(a);

    // Act
    File actualZipfile = zipResource.getZipfile();

    // Assert
    assertEquals("Name", actualZipfile.getName());
    assertTrue(actualZipfile.isAbsolute());
  }

  /**
   * Test {@link ZipResource#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link ZipResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured() {
    // Arrange
    ZipResource zipResource = new ZipResource();
    FileResource a = new FileResource();

    // Act
    zipResource.addConfigured(a);

    // Assert
    assertEquals("ZipResource \"(unbound file resource):null\"", zipResource.toLongString());
    assertNull(zipResource.getZipfile());
    assertSame(a, zipResource.getArchive());
  }

  /**
   * Test {@link ZipResource#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link ZipResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured2() {
    // Arrange
    ZipResource zipResource = new ZipResource();

    Concat a = new Concat();
    a.setDest(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    a.addFilelist(new FileList());

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.addConfigured(a));
  }

  /**
   * Test {@link ZipResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@code At least one resource must be provided, or some text.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenAtLeastOneResourceMustBeProvidedOrSomeText() {
    // Arrange
    ZipResource zipResource = new ZipResource();

    Concat a = new Concat();
    a.addText("At least one resource must be provided, or some text.");

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.addConfigured(a));
  }

  /**
   * Test {@link ZipResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileList#FileList()}.</li>
   *   <li>When {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileList_whenConcatAddFilelistFileList() {
    // Arrange
    ZipResource zipResource = new ZipResource();

    Concat a = new Concat();
    a.addFilelist(new FileList());

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.addConfigured(a));
  }

  /**
   * Test {@link ZipResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code only filesystem resources are supported}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsOnlyFilesystemResourcesAreSupported() {
    // Arrange
    ZipResource zipResource = new ZipResource();

    FileName name = new FileName();
    name.setName("only filesystem resources are supported");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat a = new Concat();
    a.addFilelist(list);

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.addConfigured(a));
  }

  /**
   * Test {@link ZipResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link FileResource#FileResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileResource_whenConcatDestIsFileResource() {
    // Arrange
    ZipResource zipResource = new ZipResource();

    Concat a = new Concat();
    a.setDest(new FileResource());
    a.addFilelist(new FileList());

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.addConfigured(a));
  }

  /**
   * Test {@link ZipResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resource#Resource(String)} with name is {@code only filesystem resources are supported}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenResourceWithNameIsOnlyFilesystemResourcesAreSupported() {
    // Arrange
    ZipResource zipResource = new ZipResource();

    Concat a = new Concat();
    a.setDest(new Resource("only filesystem resources are supported"));
    a.addFilelist(new FileList());

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.addConfigured(a));
  }

  /**
   * Test {@link ZipResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenResource_whenConcatDestIsResource_thenThrowBuildException() {
    // Arrange
    ZipResource zipResource = new ZipResource();

    Concat a = new Concat();
    a.setDest(new Resource());
    a.addFilelist(new FileList());

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.addConfigured(a));
  }

  /**
   * Test {@link ZipResource#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link ZipResource#ZipResource()}.</li>
   *   <li>When {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenZipResource_whenBZip2Resource_thenThrowBuildException() {
    // Arrange
    ZipResource zipResource = new ZipResource();

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.addConfigured(new BZip2Resource()));
  }

  /**
   * Test {@link ZipResource#setEncoding(String)}.
   * <p>
   * Method under test: {@link ZipResource#setEncoding(String)}
   */
  @Test
  public void testSetEncoding() {
    // Arrange
    ZipResource zipResource = new ZipResource();

    // Act
    zipResource.setEncoding("Enc");

    // Assert
    assertEquals("Enc", zipResource.getEncoding());
  }

  /**
   * Test {@link ZipResource#getEncoding()}.
   * <p>
   * Method under test: {@link ZipResource#getEncoding()}
   */
  @Test
  public void testGetEncoding() {
    // Arrange, Act and Assert
    assertNull((new ZipResource()).getEncoding());
  }

  /**
   * Test {@link ZipResource#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link ZipResource#ZipResource()}.</li>
   *   <li>Then {@link ZipResource#ZipResource()} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenZipResource_thenZipResourceReference() {
    // Arrange
    ZipResource zipResource = new ZipResource();
    Reference r = new Reference("42");

    // Act
    zipResource.setRefid(r);

    // Assert
    assertTrue(zipResource.isReference());
    assertSame(r, zipResource.getRefid());
  }

  /**
   * Test {@link ZipResource#getOutputStream()}.
   * <p>
   * Method under test: {@link ZipResource#getOutputStream()}
   */
  @Test
  public void testGetOutputStream() throws IOException {
    // Arrange, Act and Assert
    assertThrows(UnsupportedOperationException.class, () -> (new ZipResource()).getOutputStream());
  }

  /**
   * Test {@link ZipResource#getExtraFields()}.
   * <p>
   * Method under test: {@link ZipResource#getExtraFields()}
   */
  @Test
  public void testGetExtraFields() {
    // Arrange, Act and Assert
    assertEquals(0, (new ZipResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "entry name not set", null)).getExtraFields().length);
  }

  /**
   * Test {@link ZipResource#getExtraFields()}.
   * <ul>
   *   <li>Given {@link ZipEntry#ZipEntry(String)} with name is {@code entry name not set}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#getExtraFields()}
   */
  @Test
  public void testGetExtraFields_givenZipEntryWithNameIsEntryNameNotSet() {
    // Arrange
    File z = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals(0,
        (new ZipResource(z, "entry name not set", new ZipEntry("entry name not set"))).getExtraFields().length);
  }

  /**
   * Test {@link ZipResource#getMethod()}.
   * <p>
   * Method under test: {@link ZipResource#getMethod()}
   */
  @Test
  public void testGetMethod() {
    // Arrange, Act and Assert
    assertEquals(0, (new ZipResource()).getMethod());
  }

  /**
   * Test {@link ZipResource#fetchEntry()}.
   * <p>
   * Method under test: {@link ZipResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry() {
    // Arrange
    File z = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> (new ZipResource(z, "Enc", new ZipEntry("Name"))).fetchEntry());
  }

  /**
   * Test {@link ZipResource#fetchEntry()}.
   * <p>
   * Method under test: {@link ZipResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry2() {
    // Arrange
    ZipResource zipResource = new ZipResource();
    zipResource.setArchive(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.fetchEntry());
  }

  /**
   * Test {@link ZipResource#fetchEntry()}.
   * <p>
   * Method under test: {@link ZipResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry3() {
    // Arrange
    File z = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> (new ZipResource(z, "UTF8", new ZipEntry("Name"))).fetchEntry());
  }

  /**
   * Test {@link ZipResource#fetchEntry()}.
   * <p>
   * Method under test: {@link ZipResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry4() {
    // Arrange
    FileResource a = new FileResource();
    a.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    a.setName("Name");

    ZipResource zipResource = new ZipResource();
    zipResource.addConfigured(a);

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.fetchEntry());
  }

  /**
   * Test {@link ZipResource#fetchEntry()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry_givenFileResourceNameIsDotDot_thenThrowBuildException() {
    // Arrange
    FileResource a = new FileResource();
    a.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    a.setName("..");

    ZipResource zipResource = new ZipResource();
    zipResource.addConfigured(a);

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.fetchEntry());
  }

  /**
   * Test {@link ZipResource#fetchEntry()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code Name}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipResource#fetchEntry()}
   */
  @Test
  public void testFetchEntry_givenFileResourceNameIsName_thenThrowBuildException() {
    // Arrange
    FileResource a = new FileResource();
    a.setName("Name");

    ZipResource zipResource = new ZipResource();
    zipResource.addConfigured(a);

    // Act and Assert
    assertThrows(BuildException.class, () -> zipResource.fetchEntry());
  }
}
