package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.Hashtable;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.ZipFileSet;
import org.apache.tools.zip.Zip64Mode;
import org.apache.tools.zip.ZipEntry;
import org.apache.tools.zip.ZipOutputStream;
import org.junit.Test;

public class WarDiffblueTest {
  /**
   * Test new {@link War} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link War}
   */
  @Test
  public void testNewWar() {
    // Arrange and Act
    War actualWar = new War();

    // Assert
    assertEquals("", actualWar.getComment());
    assertEquals("UTF8", actualWar.getEncoding());
    assertEquals("add", actualWar.duplicate);
    assertEquals("create", actualWar.emptyBehavior);
    assertEquals("war", actualWar.archiveType);
    assertNull(actualWar.getCurrentExtraFields());
    assertNull(actualWar.getDestFile());
    assertNull(actualWar.getDescription());
    assertNull(actualWar.getTaskName());
    assertNull(actualWar.getTaskType());
    assertNull(actualWar.getModificationtime());
    assertNull(actualWar.getProject());
    assertNull(actualWar.getOwningTarget());
    assertNull(actualWar.getIndexJarsMapper());
    assertEquals(-1, actualWar.getLevel());
    assertFalse(actualWar.hasSelectors());
    assertFalse(actualWar.getFallBackToUTF8());
    assertFalse(actualWar.getPreserve0Permissions());
    assertFalse(actualWar.hasUpdatedFile());
    assertFalse(actualWar.isAddingNewFiles());
    assertFalse(actualWar.isInUpdateMode());
    assertFalse(actualWar.doubleFilePass);
    assertFalse(actualWar.skipWriting);
    assertTrue(actualWar.addedDirs.isEmpty());
    assertTrue(actualWar.entries.isEmpty());
    assertTrue(actualWar.getUseLanguageEnodingFlag());
    assertTrue(actualWar.isCompress());
    assertTrue(actualWar.isFirstPass());
  }

  /**
   * Test {@link War#setWarfile(File)}.
   * <p>
   * Method under test: {@link War#setWarfile(File)}
   */
  @Test
  public void testSetWarfile() {
    // Arrange
    War war = new War();
    File warFile = Copy.NULL_FILE_PLACEHOLDER;

    // Act
    war.setWarfile(warFile);

    // Assert
    assertSame(warFile, war.getDestFile());
  }

  /**
   * Test {@link War#setWebxml(File)}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link War#setWebxml(File)}
   */
  @Test
  public void testSetWebxml_whenNull_file_placeholder_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new War()).setWebxml(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link War#addLib(ZipFileSet)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link ZipFileSet#ZipFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link War#addLib(ZipFileSet)}
   */
  @Test
  public void testAddLib_givenProject_whenZipFileSetProjectIsProject() {
    // Arrange
    War war = new War();

    ZipFileSet fs = new ZipFileSet();
    fs.setProject(new Project());

    // Act
    war.addLib(fs);

    // Assert
    assertEquals("WEB-INF/lib/", fs.getPrefix());
  }

  /**
   * Test {@link War#addLib(ZipFileSet)}.
   * <ul>
   *   <li>When {@link ZipFileSet#ZipFileSet()}.</li>
   *   <li>Then {@link ZipFileSet#ZipFileSet()} Prefix is {@code WEB-INF/lib/}.</li>
   * </ul>
   * <p>
   * Method under test: {@link War#addLib(ZipFileSet)}
   */
  @Test
  public void testAddLib_whenZipFileSet_thenZipFileSetPrefixIsWebInfLib() {
    // Arrange
    War war = new War();
    ZipFileSet fs = new ZipFileSet();

    // Act
    war.addLib(fs);

    // Assert
    assertEquals("WEB-INF/lib/", fs.getPrefix());
  }

  /**
   * Test {@link War#addClasses(ZipFileSet)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link ZipFileSet#ZipFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link War#addClasses(ZipFileSet)}
   */
  @Test
  public void testAddClasses_givenProject_whenZipFileSetProjectIsProject() {
    // Arrange
    War war = new War();

    ZipFileSet fs = new ZipFileSet();
    fs.setProject(new Project());

    // Act
    war.addClasses(fs);

    // Assert
    assertEquals("WEB-INF/classes/", fs.getPrefix());
  }

  /**
   * Test {@link War#addClasses(ZipFileSet)}.
   * <ul>
   *   <li>When {@link ZipFileSet#ZipFileSet()}.</li>
   *   <li>Then {@link ZipFileSet#ZipFileSet()} Prefix is {@code WEB-INF/classes/}.</li>
   * </ul>
   * <p>
   * Method under test: {@link War#addClasses(ZipFileSet)}
   */
  @Test
  public void testAddClasses_whenZipFileSet_thenZipFileSetPrefixIsWebInfClasses() {
    // Arrange
    War war = new War();
    ZipFileSet fs = new ZipFileSet();

    // Act
    war.addClasses(fs);

    // Assert
    assertEquals("WEB-INF/classes/", fs.getPrefix());
  }

  /**
   * Test {@link War#addWebinf(ZipFileSet)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link ZipFileSet#ZipFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link War#addWebinf(ZipFileSet)}
   */
  @Test
  public void testAddWebinf_givenProject_whenZipFileSetProjectIsProject() {
    // Arrange
    War war = new War();

    ZipFileSet fs = new ZipFileSet();
    fs.setProject(new Project());

    // Act
    war.addWebinf(fs);

    // Assert
    assertEquals("WEB-INF/", fs.getPrefix());
  }

  /**
   * Test {@link War#addWebinf(ZipFileSet)}.
   * <ul>
   *   <li>When {@link ZipFileSet#ZipFileSet()}.</li>
   *   <li>Then {@link ZipFileSet#ZipFileSet()} Prefix is {@code WEB-INF/}.</li>
   * </ul>
   * <p>
   * Method under test: {@link War#addWebinf(ZipFileSet)}
   */
  @Test
  public void testAddWebinf_whenZipFileSet_thenZipFileSetPrefixIsWebInf() {
    // Arrange
    War war = new War();
    ZipFileSet fs = new ZipFileSet();

    // Act
    war.addWebinf(fs);

    // Assert
    assertEquals("WEB-INF/", fs.getPrefix());
  }

  /**
   * Test {@link War#initZipOutputStream(ZipOutputStream)}.
   * <p>
   * Method under test: {@link War#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream() throws IOException, BuildException {
    // Arrange
    War war = new War();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.putNextEntry(new ZipEntry("/org/apache/tools/ant/defaultManifest.mf"));

    // Act
    war.initZipOutputStream(zOut);

    // Assert
    Hashtable<String, String> stringStringMap = war.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = war.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link War#initZipOutputStream(ZipOutputStream)}.
   * <p>
   * Method under test: {@link War#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream2() throws IOException, BuildException {
    // Arrange
    War war = new War();
    war.setModificationtime("/org/apache/tools/ant/defaultManifest.mf");

    // Act
    war.initZipOutputStream(new ZipOutputStream(new ByteArrayOutputStream(1)));

    // Assert
    Hashtable<String, String> stringStringMap = war.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = war.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link War#initZipOutputStream(ZipOutputStream)}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link War#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream_givenAlways() throws IOException, BuildException {
    // Arrange
    War war = new War();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);
    zOut.putNextEntry(new ZipEntry("/org/apache/tools/ant/defaultManifest.mf"));

    // Act
    war.initZipOutputStream(zOut);

    // Assert
    Hashtable<String, String> stringStringMap = war.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = war.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link War#initZipOutputStream(ZipOutputStream)}.
   * <ul>
   *   <li>Given {@code Always}.</li>
   * </ul>
   * <p>
   * Method under test: {@link War#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream_givenAlways2() throws IOException, BuildException {
    // Arrange
    War war = new War();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setUseZip64(Zip64Mode.Always);

    // Act
    war.initZipOutputStream(zOut);

    // Assert
    Hashtable<String, String> stringStringMap = war.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = war.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link War#initZipOutputStream(ZipOutputStream)}.
   * <ul>
   *   <li>Given {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link War#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream_givenNull() throws IOException, BuildException {
    // Arrange
    War war = new War();

    ZipOutputStream zOut = new ZipOutputStream(new ByteArrayOutputStream(1));
    zOut.setCreateUnicodeExtraFields(null);
    zOut.putNextEntry(new ZipEntry("/org/apache/tools/ant/defaultManifest.mf"));

    // Act
    war.initZipOutputStream(zOut);

    // Assert
    Hashtable<String, String> stringStringMap = war.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = war.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }

  /**
   * Test {@link War#initZipOutputStream(ZipOutputStream)}.
   * <ul>
   *   <li>When {@link ZipOutputStream#ZipOutputStream(OutputStream)} with out is {@link ByteArrayOutputStream#ByteArrayOutputStream(int)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link War#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream_whenZipOutputStreamWithOutIsByteArrayOutputStream()
      throws IOException, BuildException {
    // Arrange
    War war = new War();

    // Act
    war.initZipOutputStream(new ZipOutputStream(new ByteArrayOutputStream(1)));

    // Assert
    Hashtable<String, String> stringStringMap = war.addedDirs;
    assertEquals(1, stringStringMap.size());
    assertEquals("META-INF/", stringStringMap.get("META-INF/"));
    Hashtable<String, String> stringStringMap2 = war.entries;
    assertEquals(1, stringStringMap2.size());
    assertEquals("META-INF/MANIFEST.MF", stringStringMap2.get("META-INF/MANIFEST.MF"));
  }
}
