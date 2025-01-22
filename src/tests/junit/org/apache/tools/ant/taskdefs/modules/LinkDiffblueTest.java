package org.apache.tools.ant.taskdefs.modules;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.modules.Link.Compression;
import org.apache.tools.ant.taskdefs.modules.Link.CompressionLevel;
import org.apache.tools.ant.taskdefs.modules.Link.Endianness;
import org.apache.tools.ant.taskdefs.modules.Link.Launcher;
import org.apache.tools.ant.taskdefs.modules.Link.LocaleSpec;
import org.apache.tools.ant.taskdefs.modules.Link.ModuleSpec;
import org.apache.tools.ant.taskdefs.modules.Link.PatternListEntry;
import org.apache.tools.ant.taskdefs.modules.Link.ReleaseInfo;
import org.apache.tools.ant.taskdefs.modules.Link.ReleaseInfoEntry;
import org.apache.tools.ant.taskdefs.modules.Link.ReleaseInfoKey;
import org.apache.tools.ant.taskdefs.modules.Link.VMType;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.LogLevel;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class LinkDiffblueTest {
  /**
   * Test Compression {@link Compression#createFiles()}.
   * <p>
   * Method under test: {@link Compression#createFiles()}
   */
  @Test
  public void testCompressionCreateFiles() {
    // Arrange and Act
    PatternListEntry actualCreateFilesResult = ((new Link()).new Compression()).createFiles();

    // Assert
    assertEquals("@null", actualCreateFilesResult.toOptionValue());
    assertNull(actualCreateFilesResult.getListFile());
    assertNull(actualCreateFilesResult.getPattern());
  }

  /**
   * Test Compression getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Compression#Compression(Link)}
   *   <li>{@link Compression#setLevel(CompressionLevel)}
   *   <li>{@link Compression#getLevel()}
   * </ul>
   */
  @Test
  public void testCompressionGettersAndSetters() {
    // Arrange and Act
    Compression actualCompression = (new Link()).new Compression();
    CompressionLevel level = new CompressionLevel();
    actualCompression.setLevel(level);

    // Assert
    assertSame(level, actualCompression.getLevel());
  }

  /**
   * Test CompressionLevel {@link CompressionLevel#getValues()}.
   * <p>
   * Method under test: {@link CompressionLevel#getValues()}
   */
  @Test
  public void testCompressionLevelGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"0", "1", "2", "none", "strings", "zip"}, (new CompressionLevel()).getValues());
  }

  /**
   * Test CompressionLevel new {@link CompressionLevel} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CompressionLevel}
   */
  @Test
  public void testCompressionLevelNewCompressionLevel() {
    // Arrange and Act
    CompressionLevel actualCompressionLevel = new CompressionLevel();

    // Assert
    assertNull(actualCompressionLevel.getValue());
    assertEquals(-1, actualCompressionLevel.getIndex());
  }

  /**
   * Test CompressionLevel {@link CompressionLevel#toCommandLineOption()}.
   * <p>
   * Method under test: {@link CompressionLevel#toCommandLineOption()}
   */
  @Test
  public void testCompressionLevelToCommandLineOption() {
    // Arrange, Act and Assert
    assertNull((new CompressionLevel()).toCommandLineOption());
  }

  /**
   * Test Compression {@link Compression#validate()}.
   * <ul>
   *   <li>Given {@link Compression#Compression(Link)} with this$0 is {@link Link} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Compression#validate()}
   */
  @Test
  public void testCompressionValidate_givenCompressionWithThis$0IsLink_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Link()).new Compression()).validate());
  }

  /**
   * Test {@link Link#createModulePath()}.
   * <ul>
   *   <li>Given {@link Link} (default constructor).</li>
   *   <li>Then {@link Link} (default constructor) ModulePath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Link#createModulePath()}
   */
  @Test
  public void testCreateModulePath_givenLink_thenLinkModulePathDescriptionIsNull() {
    // Arrange
    Link link = new Link();

    // Act
    Path actualCreateModulePathResult = link.createModulePath();

    // Assert
    Path modulePath = link.getModulePath();
    assertNull(modulePath.getDescription());
    assertNull(actualCreateModulePathResult.getProject());
    assertNull(modulePath.getProject());
    assertNull(modulePath.getRefid());
    assertEquals(0, modulePath.size());
    assertFalse(modulePath.isReference());
    assertTrue(modulePath.isEmpty());
  }

  /**
   * Test {@link Link#createModulePath()}.
   * <ul>
   *   <li>Then {@link Link} (default constructor) ModulePath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Link#createModulePath()}
   */
  @Test
  public void testCreateModulePath_thenLinkModulePathIsSystemBootClasspath() {
    // Arrange
    Link link = new Link();
    link.setModulePath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedModulePath = link.createModulePath().systemBootClasspath;
    assertSame(expectedModulePath, link.getModulePath());
  }

  /**
   * Test Endianness {@link Endianness#getValues()}.
   * <p>
   * Method under test: {@link Endianness#getValues()}
   */
  @Test
  public void testEndiannessGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"little", "big"}, (new Endianness()).getValues());
  }

  /**
   * Test Endianness new {@link Endianness} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Endianness}
   */
  @Test
  public void testEndiannessNewEndianness() {
    // Arrange and Act
    Endianness actualEndianness = new Endianness();

    // Assert
    assertNull(actualEndianness.getValue());
    assertEquals(-1, actualEndianness.getIndex());
  }

  /**
   * Test Launcher getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Launcher#Launcher(Link)}
   *   <li>{@link Launcher#setMainClass(String)}
   *   <li>{@link Launcher#setModule(String)}
   *   <li>{@link Launcher#setName(String)}
   *   <li>{@link Launcher#getMainClass()}
   *   <li>{@link Launcher#getModule()}
   *   <li>{@link Launcher#getName()}
   * </ul>
   */
  @Test
  public void testLauncherGettersAndSetters() {
    // Arrange and Act
    Launcher actualLauncher = (new Link()).new Launcher();
    actualLauncher.setMainClass("Class Name");
    actualLauncher.setModule("Module");
    actualLauncher.setName("Name");
    String actualMainClass = actualLauncher.getMainClass();
    String actualModule = actualLauncher.getModule();

    // Assert
    assertEquals("Class Name", actualMainClass);
    assertEquals("Module", actualModule);
    assertEquals("Name", actualLauncher.getName());
  }

  /**
   * Test Launcher {@link Launcher#Launcher(Link, String)}.
   * <ul>
   *   <li>Then return {@code mainclass}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Launcher#Launcher(Link, String)}
   */
  @Test
  public void testLauncherNewLauncher_thenReturnMainclass() {
    // Arrange
    Link link = new Link();

    // Act
    Launcher actualLauncher = link.new Launcher(String.join("", System.getProperty("apple.awt.application.name"),
        " command must take the form name=module or name=module/mainclass"));

    // Assert
    assertEquals("mainclass", actualLauncher.getMainClass());
    assertEquals("module", actualLauncher.getModule());
    String expectedName = String.join("", System.getProperty("apple.awt.application.name"),
        " command must take the form name=module or name");
    assertEquals(expectedName, actualLauncher.getName());
  }

  /**
   * Test Launcher {@link Launcher#Launcher(Link, String)}.
   * <ul>
   *   <li>When {@code Text Spec}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Launcher#Launcher(Link, String)}
   */
  @Test
  public void testLauncherNewLauncher_whenTextSpec_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Link()).new Launcher("Text Spec"));

  }

  /**
   * Test Launcher {@link Launcher#toString()}.
   * <ul>
   *   <li>Given {@link Launcher#Launcher(Link)} with this$0 is {@link Link} (default constructor).</li>
   *   <li>Then return {@code null=null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Launcher#toString()}
   */
  @Test
  public void testLauncherToString_givenLauncherWithThis$0IsLink_thenReturnNullNull() {
    // Arrange, Act and Assert
    assertEquals("null=null", ((new Link()).new Launcher()).toString());
  }

  /**
   * Test Launcher {@link Launcher#toString()}.
   * <ul>
   *   <li>Then return {@code null=null/foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Launcher#toString()}
   */
  @Test
  public void testLauncherToString_thenReturnNullNullFoo() {
    // Arrange
    Launcher launcher = (new Link()).new Launcher();
    launcher.setMainClass("foo");

    // Act and Assert
    assertEquals("null=null/foo", launcher.toString());
  }

  /**
   * Test Launcher {@link Launcher#validate()}.
   * <ul>
   *   <li>Given {@link Launcher#Launcher(Link)} with this$0 is {@link Link} (default constructor) Module is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Launcher#validate()}
   */
  @Test
  public void testLauncherValidate_givenLauncherWithThis$0IsLinkModuleIsEmptyString() {
    // Arrange
    Launcher launcher = (new Link()).new Launcher();
    launcher.setName("foo");
    launcher.setModule("");

    // Act and Assert
    assertThrows(BuildException.class, () -> launcher.validate());
  }

  /**
   * Test Launcher {@link Launcher#validate()}.
   * <ul>
   *   <li>Given {@link Launcher#Launcher(Link)} with this$0 is {@link Link} (default constructor) Module is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Launcher#validate()}
   */
  @Test
  public void testLauncherValidate_givenLauncherWithThis$0IsLinkModuleIsNull() {
    // Arrange
    Launcher launcher = (new Link()).new Launcher();
    launcher.setName("foo");
    launcher.setModule(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> launcher.validate());
  }

  /**
   * Test Launcher {@link Launcher#validate()}.
   * <ul>
   *   <li>Given {@link Launcher#Launcher(Link)} with this$0 is {@link Link} (default constructor) Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Launcher#validate()}
   */
  @Test
  public void testLauncherValidate_givenLauncherWithThis$0IsLinkNameIsEmptyString() {
    // Arrange
    Launcher launcher = (new Link()).new Launcher();
    launcher.setName("");
    launcher.setModule(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> launcher.validate());
  }

  /**
   * Test Launcher {@link Launcher#validate()}.
   * <ul>
   *   <li>Given {@link Launcher#Launcher(Link)} with this$0 is {@link Link} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Launcher#validate()}
   */
  @Test
  public void testLauncherValidate_givenLauncherWithThis$0IsLink_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Link()).new Launcher()).validate());
  }

  /**
   * Test LocaleSpec getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LocaleSpec#LocaleSpec(Link)}
   *   <li>{@link LocaleSpec#setName(String)}
   *   <li>{@link LocaleSpec#getName()}
   * </ul>
   */
  @Test
  public void testLocaleSpecGettersAndSetters() {
    // Arrange and Act
    LocaleSpec actualLocaleSpec = (new Link()).new LocaleSpec();
    actualLocaleSpec.setName("en");

    // Assert
    assertEquals("en", actualLocaleSpec.getName());
  }

  /**
   * Test LocaleSpec {@link LocaleSpec#LocaleSpec(Link, String)}.
   * <p>
   * Method under test: {@link LocaleSpec#LocaleSpec(Link, String)}
   */
  @Test
  public void testLocaleSpecNewLocaleSpec() {
    // Arrange, Act and Assert
    assertEquals("en", ((new Link()).new LocaleSpec("en")).getName());
  }

  /**
   * Test LocaleSpec {@link LocaleSpec#validate()}.
   * <ul>
   *   <li>Given {@link LocaleSpec#LocaleSpec(Link)} with this$0 is {@link Link} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LocaleSpec#validate()}
   */
  @Test
  public void testLocaleSpecValidate_givenLocaleSpecWithThis$0IsLink_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Link()).new LocaleSpec()).validate());
  }

  /**
   * Test ModuleSpec getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ModuleSpec#ModuleSpec(Link)}
   *   <li>{@link ModuleSpec#setName(String)}
   *   <li>{@link ModuleSpec#getName()}
   * </ul>
   */
  @Test
  public void testModuleSpecGettersAndSetters() {
    // Arrange and Act
    ModuleSpec actualModuleSpec = (new Link()).new ModuleSpec();
    actualModuleSpec.setName("Name");

    // Assert
    assertEquals("Name", actualModuleSpec.getName());
  }

  /**
   * Test ModuleSpec {@link ModuleSpec#ModuleSpec(Link, String)}.
   * <p>
   * Method under test: {@link ModuleSpec#ModuleSpec(Link, String)}
   */
  @Test
  public void testModuleSpecNewModuleSpec() {
    // Arrange, Act and Assert
    assertEquals("Name", ((new Link()).new ModuleSpec("Name")).getName());
  }

  /**
   * Test ModuleSpec {@link ModuleSpec#validate()}.
   * <ul>
   *   <li>Given {@link ModuleSpec#ModuleSpec(Link)} with this$0 is {@link Link} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ModuleSpec#validate()}
   */
  @Test
  public void testModuleSpecValidate_givenModuleSpecWithThis$0IsLink_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Link()).new ModuleSpec()).validate());
  }

  /**
   * Test PatternListEntry getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link PatternListEntry#PatternListEntry(Link)}
   *   <li>{@link PatternListEntry#setListFile(File)}
   *   <li>{@link PatternListEntry#setPattern(String)}
   *   <li>{@link PatternListEntry#getListFile()}
   *   <li>{@link PatternListEntry#getPattern()}
   * </ul>
   */
  @Test
  public void testPatternListEntryGettersAndSetters() {
    // Arrange and Act
    PatternListEntry actualPatternListEntry = (new Link()).new PatternListEntry();
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    actualPatternListEntry.setListFile(file);
    actualPatternListEntry.setPattern("Pattern");
    File actualListFile = actualPatternListEntry.getListFile();

    // Assert
    assertEquals("Pattern", actualPatternListEntry.getPattern());
    assertSame(file, actualListFile);
  }

  /**
   * Test PatternListEntry {@link PatternListEntry#PatternListEntry(Link, String)}.
   * <ul>
   *   <li>Then return ListFile Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternListEntry#PatternListEntry(Link, String)}
   */
  @Test
  public void testPatternListEntryNewPatternListEntry_thenReturnListFileNameIsEmptyString() {
    // Arrange and Act
    PatternListEntry actualPatternListEntry = (new Link()).new PatternListEntry("@");

    // Assert
    File listFile = actualPatternListEntry.getListFile();
    assertEquals("", listFile.getName());
    assertEquals("@", actualPatternListEntry.toOptionValue());
    assertNull(actualPatternListEntry.getPattern());
    assertFalse(listFile.isAbsolute());
  }

  /**
   * Test PatternListEntry {@link PatternListEntry#PatternListEntry(Link, String)}.
   * <ul>
   *   <li>When {@code Pattern}.</li>
   *   <li>Then return {@code Pattern}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternListEntry#PatternListEntry(Link, String)}
   */
  @Test
  public void testPatternListEntryNewPatternListEntry_whenPattern_thenReturnPattern() {
    // Arrange and Act
    PatternListEntry actualPatternListEntry = (new Link()).new PatternListEntry("Pattern");

    // Assert
    assertEquals("Pattern", actualPatternListEntry.getPattern());
    assertEquals("Pattern", actualPatternListEntry.toOptionValue());
    assertNull(actualPatternListEntry.getListFile());
  }

  /**
   * Test PatternListEntry {@link PatternListEntry#toOptionValue()}.
   * <ul>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternListEntry#toOptionValue()}
   */
  @Test
  public void testPatternListEntryToOptionValue_thenReturnFoo() {
    // Arrange
    PatternListEntry patternListEntry = (new Link()).new PatternListEntry();
    patternListEntry.setPattern("foo");

    // Act and Assert
    assertEquals("foo", patternListEntry.toOptionValue());
  }

  /**
   * Test PatternListEntry {@link PatternListEntry#toOptionValue()}.
   * <ul>
   *   <li>Then return {@code @null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternListEntry#toOptionValue()}
   */
  @Test
  public void testPatternListEntryToOptionValue_thenReturnNull() {
    // Arrange, Act and Assert
    assertEquals("@null", ((new Link()).new PatternListEntry()).toOptionValue());
  }

  /**
   * Test PatternListEntry {@link PatternListEntry#validate()}.
   * <ul>
   *   <li>Given {@link PatternListEntry#PatternListEntry(Link)} with this$0 is {@link Link} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternListEntry#validate()}
   */
  @Test
  public void testPatternListEntryValidate_givenPatternListEntryWithThis$0IsLink() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Link()).new PatternListEntry()).validate());
  }

  /**
   * Test PatternListEntry {@link PatternListEntry#validate()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PatternListEntry#validate()}
   */
  @Test
  public void testPatternListEntryValidate_thenThrowBuildException() {
    // Arrange
    PatternListEntry patternListEntry = (new Link()).new PatternListEntry();
    patternListEntry.setPattern("foo");
    patternListEntry.setListFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> patternListEntry.validate());
  }

  /**
   * Test ReleaseInfo {@link ReleaseInfo#createAdd()}.
   * <p>
   * Method under test: {@link ReleaseInfo#createAdd()}
   */
  @Test
  public void testReleaseInfoCreateAdd() {
    // Arrange and Act
    ReleaseInfoEntry actualCreateAddResult = ((new Link()).new ReleaseInfo()).createAdd();

    // Assert
    assertEquals("ISO-8859-1", actualCreateAddResult.getCharset());
    assertNull(actualCreateAddResult.getFile());
    assertNull(actualCreateAddResult.getKey());
    assertNull(actualCreateAddResult.getValue());
  }

  /**
   * Test ReleaseInfo {@link ReleaseInfo#createDelete()}.
   * <p>
   * Method under test: {@link ReleaseInfo#createDelete()}
   */
  @Test
  public void testReleaseInfoCreateDelete() {
    // Arrange, Act and Assert
    assertNull(((new Link()).new ReleaseInfo()).createDelete().getKey());
  }

  /**
   * Test ReleaseInfoEntry getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ReleaseInfoEntry#setCharset(String)}
   *   <li>{@link ReleaseInfoEntry#setFile(File)}
   *   <li>{@link ReleaseInfoEntry#setKey(String)}
   *   <li>{@link ReleaseInfoEntry#setValue(String)}
   *   <li>{@link ReleaseInfoEntry#getCharset()}
   *   <li>{@link ReleaseInfoEntry#getFile()}
   *   <li>{@link ReleaseInfoEntry#getKey()}
   *   <li>{@link ReleaseInfoEntry#getValue()}
   * </ul>
   */
  @Test
  public void testReleaseInfoEntryGettersAndSetters() {
    // Arrange
    ReleaseInfoEntry releaseInfoEntry = (new Link()).new ReleaseInfoEntry();

    // Act
    releaseInfoEntry.setCharset("UTF-8");
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    releaseInfoEntry.setFile(file);
    releaseInfoEntry.setKey("Key");
    releaseInfoEntry.setValue("42");
    String actualCharset = releaseInfoEntry.getCharset();
    File actualFile = releaseInfoEntry.getFile();
    String actualKey = releaseInfoEntry.getKey();

    // Assert
    assertEquals("42", releaseInfoEntry.getValue());
    assertEquals("Key", actualKey);
    assertEquals("UTF-8", actualCharset);
    assertSame(file, actualFile);
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#ReleaseInfoEntry(Link)}.
   * <p>
   * Method under test: {@link ReleaseInfoEntry#ReleaseInfoEntry(Link)}
   */
  @Test
  public void testReleaseInfoEntryNewReleaseInfoEntry() {
    // Arrange and Act
    ReleaseInfoEntry actualReleaseInfoEntry = (new Link()).new ReleaseInfoEntry();

    // Assert
    assertEquals("ISO-8859-1", actualReleaseInfoEntry.getCharset());
    assertNull(actualReleaseInfoEntry.getFile());
    assertNull(actualReleaseInfoEntry.getKey());
    assertNull(actualReleaseInfoEntry.getValue());
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#ReleaseInfoEntry(Link, String, String)}.
   * <p>
   * Method under test: {@link ReleaseInfoEntry#ReleaseInfoEntry(Link, String, String)}
   */
  @Test
  public void testReleaseInfoEntryNewReleaseInfoEntry2() {
    // Arrange and Act
    ReleaseInfoEntry actualReleaseInfoEntry = (new Link()).new ReleaseInfoEntry("Key", "42");

    // Assert
    assertEquals("42", actualReleaseInfoEntry.getValue());
    assertEquals("ISO-8859-1", actualReleaseInfoEntry.getCharset());
    assertEquals("Key", actualReleaseInfoEntry.getKey());
    assertNull(actualReleaseInfoEntry.getFile());
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#toProperties()}.
   * <p>
   * Method under test: {@link ReleaseInfoEntry#toProperties()}
   */
  @Test
  public void testReleaseInfoEntryToProperties() {
    // Arrange
    ReleaseInfoEntry releaseInfoEntry = (new Link()).new ReleaseInfoEntry();
    releaseInfoEntry.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> releaseInfoEntry.toProperties());
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#toProperties()}.
   * <p>
   * Method under test: {@link ReleaseInfoEntry#toProperties()}
   */
  @Test
  public void testReleaseInfoEntryToProperties2() {
    // Arrange
    ReleaseInfoEntry releaseInfoEntry = (new Link()).new ReleaseInfoEntry();
    String property = System.getProperty("java.io.tmpdir");
    releaseInfoEntry
        .setFile(Paths.get(property, String.join("", "Cannot read ", System.getProperty("jdk.debug"), " info file \""))
            .toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> releaseInfoEntry.toProperties());
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#toProperties()}.
   * <ul>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReleaseInfoEntry#toProperties()}
   */
  @Test
  public void testReleaseInfoEntryToProperties_thenReturnSizeIsOne() {
    // Arrange and Act
    Properties actualToPropertiesResult = ((new Link()).new ReleaseInfoEntry("Key", "42")).toProperties();

    // Assert
    assertEquals(1, actualToPropertiesResult.size());
    assertEquals("42", actualToPropertiesResult.get("Key"));
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#validate()}.
   * <p>
   * Method under test: {@link ReleaseInfoEntry#validate()}
   */
  @Test
  public void testReleaseInfoEntryValidate() {
    // Arrange
    ReleaseInfoEntry releaseInfoEntry = (new Link()).new ReleaseInfoEntry();
    releaseInfoEntry.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    releaseInfoEntry.setKey("foo");
    releaseInfoEntry.setValue(null);
    releaseInfoEntry.setCharset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> releaseInfoEntry.validate());
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#validate()}.
   * <ul>
   *   <li>Given {@link ReleaseInfoEntry#ReleaseInfoEntry(Link)} with this$0 is {@link Link} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ReleaseInfoEntry#validate()}
   */
  @Test
  public void testReleaseInfoEntryValidate_givenReleaseInfoEntryWithThis$0IsLink() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Link()).new ReleaseInfoEntry()).validate());
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#validate()}.
   * <ul>
   *   <li>Given {@link ReleaseInfoEntry#ReleaseInfoEntry(Link)} with this$0 is {@link Link} (default constructor) Charset is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReleaseInfoEntry#validate()}
   */
  @Test
  public void testReleaseInfoEntryValidate_givenReleaseInfoEntryWithThis$0IsLinkCharsetIsFoo() {
    // Arrange
    ReleaseInfoEntry releaseInfoEntry = (new Link()).new ReleaseInfoEntry();
    releaseInfoEntry.setFile(null);
    releaseInfoEntry.setKey("foo");
    releaseInfoEntry.setValue("foo");
    releaseInfoEntry.setCharset("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> releaseInfoEntry.validate());
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#validate()}.
   * <ul>
   *   <li>Given {@link ReleaseInfoEntry#ReleaseInfoEntry(Link)} with this$0 is {@link Link} (default constructor) File is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReleaseInfoEntry#validate()}
   */
  @Test
  public void testReleaseInfoEntryValidate_givenReleaseInfoEntryWithThis$0IsLinkFileIsNull() {
    // Arrange
    ReleaseInfoEntry releaseInfoEntry = (new Link()).new ReleaseInfoEntry();
    releaseInfoEntry.setFile(null);
    releaseInfoEntry.setKey("foo");
    releaseInfoEntry.setValue(null);
    releaseInfoEntry.setCharset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> releaseInfoEntry.validate());
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#validate()}.
   * <ul>
   *   <li>Given {@link ReleaseInfoEntry#ReleaseInfoEntry(Link)} with this$0 is {@link Link} (default constructor) File is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReleaseInfoEntry#validate()}
   */
  @Test
  public void testReleaseInfoEntryValidate_givenReleaseInfoEntryWithThis$0IsLinkFileIsNull2() {
    // Arrange
    ReleaseInfoEntry releaseInfoEntry = (new Link()).new ReleaseInfoEntry();
    releaseInfoEntry.setFile(null);
    releaseInfoEntry.setKey("foo");
    releaseInfoEntry.setValue("foo");
    releaseInfoEntry.setCharset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> releaseInfoEntry.validate());
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#validate()}.
   * <ul>
   *   <li>Given {@link ReleaseInfoEntry#ReleaseInfoEntry(Link)} with this$0 is {@link Link} (default constructor) Key is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReleaseInfoEntry#validate()}
   */
  @Test
  public void testReleaseInfoEntryValidate_givenReleaseInfoEntryWithThis$0IsLinkKeyIsNull() {
    // Arrange
    ReleaseInfoEntry releaseInfoEntry = (new Link()).new ReleaseInfoEntry();
    releaseInfoEntry.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    releaseInfoEntry.setKey(null);
    releaseInfoEntry.setValue(null);
    releaseInfoEntry.setCharset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> releaseInfoEntry.validate());
  }

  /**
   * Test ReleaseInfoEntry {@link ReleaseInfoEntry#validate()}.
   * <ul>
   *   <li>Given {@link ReleaseInfoEntry#ReleaseInfoEntry(Link)} with this$0 is {@link Link} (default constructor) Key is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReleaseInfoEntry#validate()}
   */
  @Test
  public void testReleaseInfoEntryValidate_givenReleaseInfoEntryWithThis$0IsLinkKeyIsNull2() {
    // Arrange
    ReleaseInfoEntry releaseInfoEntry = (new Link()).new ReleaseInfoEntry();
    releaseInfoEntry.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    releaseInfoEntry.setKey(null);
    releaseInfoEntry.setValue("foo");
    releaseInfoEntry.setCharset(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> releaseInfoEntry.validate());
  }

  /**
   * Test ReleaseInfo getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ReleaseInfo#ReleaseInfo(Link)}
   *   <li>{@link ReleaseInfo#setFile(File)}
   *   <li>{@link ReleaseInfo#getFile()}
   * </ul>
   */
  @Test
  public void testReleaseInfoGettersAndSetters() {
    // Arrange and Act
    ReleaseInfo actualReleaseInfo = (new Link()).new ReleaseInfo();
    File file = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    actualReleaseInfo.setFile(file);

    // Assert
    assertSame(file, actualReleaseInfo.getFile());
  }

  /**
   * Test ReleaseInfoKey getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ReleaseInfoKey#ReleaseInfoKey(Link)}
   *   <li>{@link ReleaseInfoKey#setKey(String)}
   *   <li>{@link ReleaseInfoKey#getKey()}
   * </ul>
   */
  @Test
  public void testReleaseInfoKeyGettersAndSetters() {
    // Arrange and Act
    ReleaseInfoKey actualReleaseInfoKey = (new Link()).new ReleaseInfoKey();
    actualReleaseInfoKey.setKey("Key");

    // Assert
    assertEquals("Key", actualReleaseInfoKey.getKey());
  }

  /**
   * Test ReleaseInfoKey {@link ReleaseInfoKey#ReleaseInfoKey(Link, String)}.
   * <p>
   * Method under test: {@link ReleaseInfoKey#ReleaseInfoKey(Link, String)}
   */
  @Test
  public void testReleaseInfoKeyNewReleaseInfoKey() {
    // Arrange, Act and Assert
    assertEquals("Key", ((new Link()).new ReleaseInfoKey("Key")).getKey());
  }

  /**
   * Test ReleaseInfoKey {@link ReleaseInfoKey#validate()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReleaseInfoKey#validate()}
   */
  @Test
  public void testReleaseInfoKeyValidate_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Link()).new ReleaseInfoKey()).validate());
  }

  /**
   * Test ReleaseInfo {@link ReleaseInfo#toCommandLineOptions()}.
   * <ul>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReleaseInfo#toCommandLineOptions()}
   */
  @Test
  public void testReleaseInfoToCommandLineOptions_thenReturnEmpty() {
    // Arrange and Act
    Collection<String> actualToCommandLineOptionsResult = ((new Link()).new ReleaseInfo()).toCommandLineOptions();

    // Assert
    assertTrue(actualToCommandLineOptionsResult instanceof List);
    assertTrue(actualToCommandLineOptionsResult.isEmpty());
  }

  /**
   * Test {@link Link#setModulePath(Path)}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Link#setModulePath(Path)}
   */
  @Test
  public void testSetModulePath_givenPathWithProjectIsProjectAddFilesetFileSet() throws BuildException {
    // Arrange
    Project project = new Project();

    Path path = new Path(project);
    path.addFileset(new FileSet());

    Link link = new Link();
    link.setModulePath(path);
    Path path2 = Path.systemBootClasspath;
    path2.setProject(null);

    // Act
    link.setModulePath(path2);

    // Assert
    assertSame(project, path2.getProject());
  }

  /**
   * Test {@link Link#setModulePath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Link#setModulePath(Path)}
   */
  @Test
  public void testSetModulePath_thenSystemBootClasspathProjectIsNull() {
    // Arrange
    Link link = new Link();
    link.setModulePath(new Path(null));
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    // Act
    link.setModulePath(path);

    // Assert that nothing has changed
    assertNull(path.getProject());
  }

  /**
   * Test {@link Link#setModulePath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Link#setModulePath(Path)}
   */
  @Test
  public void testSetModulePath_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    Link link = new Link();
    Project project = new Project();
    link.setModulePath(new Path(project));
    Path path = Path.systemBootClasspath;
    path.setProject(null);

    // Act
    link.setModulePath(path);

    // Assert
    assertSame(project, path.getProject());
  }

  /**
   * Test {@link Link#setModulePathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Link} (default constructor).</li>
   *   <li>Then {@link Link} (default constructor) ModulePath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Link#setModulePathRef(Reference)}
   */
  @Test
  public void testSetModulePathRef_givenLink_thenLinkModulePathProjectIsNull() {
    // Arrange
    Link link = new Link();

    // Act
    link.setModulePathRef(new Reference("42"));

    // Assert
    Path modulePath = link.getModulePath();
    assertNull(modulePath.getDescription());
    assertNull(modulePath.getProject());
    assertNull(modulePath.getRefid());
    assertFalse(modulePath.isReference());
  }

  /**
   * Test {@link Link#createModule()}.
   * <p>
   * Method under test: {@link Link#createModule()}
   */
  @Test
  public void testCreateModule() {
    // Arrange, Act and Assert
    assertNull((new Link()).createModule().getName());
  }

  /**
   * Test {@link Link#createObservableModule()}.
   * <p>
   * Method under test: {@link Link#createObservableModule()}
   */
  @Test
  public void testCreateObservableModule() {
    // Arrange, Act and Assert
    assertNull((new Link()).createObservableModule().getName());
  }

  /**
   * Test {@link Link#createLauncher()}.
   * <p>
   * Method under test: {@link Link#createLauncher()}
   */
  @Test
  public void testCreateLauncher() {
    // Arrange and Act
    Launcher actualCreateLauncherResult = (new Link()).createLauncher();

    // Assert
    assertNull(actualCreateLauncherResult.getMainClass());
    assertNull(actualCreateLauncherResult.getModule());
    assertNull(actualCreateLauncherResult.getName());
  }

  /**
   * Test {@link Link#createLocale()}.
   * <p>
   * Method under test: {@link Link#createLocale()}
   */
  @Test
  public void testCreateLocale() {
    // Arrange, Act and Assert
    assertNull((new Link()).createLocale().getName());
  }

  /**
   * Test {@link Link#createExcludeFiles()}.
   * <p>
   * Method under test: {@link Link#createExcludeFiles()}
   */
  @Test
  public void testCreateExcludeFiles() {
    // Arrange and Act
    PatternListEntry actualCreateExcludeFilesResult = (new Link()).createExcludeFiles();

    // Assert
    assertEquals("@null", actualCreateExcludeFilesResult.toOptionValue());
    assertNull(actualCreateExcludeFilesResult.getListFile());
    assertNull(actualCreateExcludeFilesResult.getPattern());
  }

  /**
   * Test {@link Link#createExcludeResources()}.
   * <p>
   * Method under test: {@link Link#createExcludeResources()}
   */
  @Test
  public void testCreateExcludeResources() {
    // Arrange and Act
    PatternListEntry actualCreateExcludeResourcesResult = (new Link()).createExcludeResources();

    // Assert
    assertEquals("@null", actualCreateExcludeResourcesResult.toOptionValue());
    assertNull(actualCreateExcludeResourcesResult.getListFile());
    assertNull(actualCreateExcludeResourcesResult.getPattern());
  }

  /**
   * Test {@link Link#createResourceOrder()}.
   * <p>
   * Method under test: {@link Link#createResourceOrder()}
   */
  @Test
  public void testCreateResourceOrder() {
    // Arrange and Act
    PatternListEntry actualCreateResourceOrderResult = (new Link()).createResourceOrder();

    // Assert
    assertEquals("@null", actualCreateResourceOrderResult.toOptionValue());
    assertNull(actualCreateResourceOrderResult.getListFile());
    assertNull(actualCreateResourceOrderResult.getPattern());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Link#setBindServices(boolean)}
   *   <li>{@link Link#setCheckDuplicateLegal(boolean)}
   *   <li>{@link Link#setCompress(CompressionLevel)}
   *   <li>{@link Link#setDebug(boolean)}
   *   <li>{@link Link#setDestDir(File)}
   *   <li>{@link Link#setEndianness(Endianness)}
   *   <li>{@link Link#setIgnoreSigning(boolean)}
   *   <li>{@link Link#setIncludeHeaders(boolean)}
   *   <li>{@link Link#setIncludeManPages(boolean)}
   *   <li>{@link Link#setIncludeNativeCommands(boolean)}
   *   <li>{@link Link#setVerboseLevel(LogLevel)}
   *   <li>{@link Link#setVmType(VMType)}
   *   <li>{@link Link#getBindServices()}
   *   <li>{@link Link#getCheckDuplicateLegal()}
   *   <li>{@link Link#getCompress()}
   *   <li>{@link Link#getDebug()}
   *   <li>{@link Link#getDestDir()}
   *   <li>{@link Link#getEndianness()}
   *   <li>{@link Link#getIgnoreSigning()}
   *   <li>{@link Link#getIncludeHeaders()}
   *   <li>{@link Link#getIncludeManPages()}
   *   <li>{@link Link#getIncludeNativeCommands()}
   *   <li>{@link Link#getModulePath()}
   *   <li>{@link Link#getVerboseLevel()}
   *   <li>{@link Link#getVmType()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Link link = new Link();

    // Act
    link.setBindServices(true);
    link.setCheckDuplicateLegal(true);
    CompressionLevel level = new CompressionLevel();
    link.setCompress(level);
    link.setDebug(true);
    File dir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();
    link.setDestDir(dir);
    Endianness endianness = new Endianness();
    link.setEndianness(endianness);
    link.setIgnoreSigning(true);
    link.setIncludeHeaders(true);
    link.setIncludeManPages(true);
    link.setIncludeNativeCommands(true);
    link.setVerboseLevel(LogLevel.DEBUG);
    VMType type = new VMType();
    link.setVmType(type);
    boolean actualBindServices = link.getBindServices();
    boolean actualCheckDuplicateLegal = link.getCheckDuplicateLegal();
    CompressionLevel actualCompress = link.getCompress();
    boolean actualDebug = link.getDebug();
    File actualDestDir = link.getDestDir();
    Endianness actualEndianness = link.getEndianness();
    boolean actualIgnoreSigning = link.getIgnoreSigning();
    boolean actualIncludeHeaders = link.getIncludeHeaders();
    boolean actualIncludeManPages = link.getIncludeManPages();
    boolean actualIncludeNativeCommands = link.getIncludeNativeCommands();
    Path actualModulePath = link.getModulePath();
    LogLevel actualVerboseLevel = link.getVerboseLevel();

    // Assert
    assertNull(actualModulePath);
    assertTrue(actualBindServices);
    assertTrue(actualCheckDuplicateLegal);
    assertTrue(actualDebug);
    assertTrue(actualIgnoreSigning);
    assertTrue(actualIncludeHeaders);
    assertTrue(actualIncludeManPages);
    assertTrue(actualIncludeNativeCommands);
    assertSame(level, actualCompress);
    assertSame(endianness, actualEndianness);
    assertSame(type, link.getVmType());
    assertSame(dir, actualDestDir);
    assertSame(actualVerboseLevel.DEBUG, actualVerboseLevel);
  }

  /**
   * Test {@link Link#createCompress()}.
   * <p>
   * Method under test: {@link Link#createCompress()}
   */
  @Test
  public void testCreateCompress() {
    // Arrange, Act and Assert
    assertNull((new Link()).createCompress().getLevel());
  }

  /**
   * Test {@link Link#createReleaseInfo()}.
   * <p>
   * Method under test: {@link Link#createReleaseInfo()}
   */
  @Test
  public void testCreateReleaseInfo() {
    // Arrange, Act and Assert
    assertNull((new Link()).createReleaseInfo().getFile());
  }

  /**
   * Test {@link Link#execute()}.
   * <ul>
   *   <li>Given {@link Link} (default constructor) ModulePath is {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Link#execute()}
   */
  @Test
  public void testExecute_givenLinkModulePathIsPathWithPIsProjectAndPath() throws BuildException {
    // Arrange
    Link link = new Link();
    link.setModulePath(new Path(new Project(), "Path"));
    link.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> link.execute());
  }

  /**
   * Test {@link Link#execute()}.
   * <ul>
   *   <li>Given {@link Link} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Link#execute()}
   */
  @Test
  public void testExecute_givenLink_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Link()).execute());
  }

  /**
   * Test {@link Link#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Link#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException() throws BuildException {
    // Arrange
    Link link = new Link();
    link.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> link.execute());
  }

  /**
   * Test {@link Link#execute()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Link#execute()}
   */
  @Test
  public void testExecute_thenThrowBuildException2() throws BuildException {
    // Arrange
    Link link = new Link();
    link.setModulePath(new Path(new Project()));
    link.setDestDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> link.execute());
  }

  /**
   * Test new {@link Link} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Link}
   */
  @Test
  public void testNewLink() {
    // Arrange and Act
    Link actualLink = new Link();

    // Assert
    assertNull(actualLink.getDestDir());
    assertNull(actualLink.getDescription());
    assertNull(actualLink.getTaskName());
    assertNull(actualLink.getTaskType());
    assertNull(actualLink.getProject());
    assertNull(actualLink.getOwningTarget());
    assertNull(actualLink.getCompress());
    assertNull(actualLink.getEndianness());
    assertNull(actualLink.getVmType());
    assertNull(actualLink.getVerboseLevel());
    assertNull(actualLink.getModulePath());
    assertFalse(actualLink.getBindServices());
    assertFalse(actualLink.getCheckDuplicateLegal());
    assertFalse(actualLink.getIgnoreSigning());
    assertTrue(actualLink.getDebug());
    assertTrue(actualLink.getIncludeHeaders());
    assertTrue(actualLink.getIncludeManPages());
    assertTrue(actualLink.getIncludeNativeCommands());
  }

  /**
   * Test VMType {@link VMType#getValues()}.
   * <p>
   * Method under test: {@link VMType#getValues()}
   */
  @Test
  public void testVMTypeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"client", "server", "minimal", "all"}, (new VMType()).getValues());
  }

  /**
   * Test VMType new {@link VMType} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link VMType}
   */
  @Test
  public void testVMTypeNewVMType() {
    // Arrange and Act
    VMType actualVmType = new VMType();

    // Assert
    assertNull(actualVmType.getValue());
    assertEquals(-1, actualVmType.getIndex());
  }
}
