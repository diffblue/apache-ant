package org.apache.tools.ant.types;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Iterator;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.Commandline.Marker;
import org.junit.Test;

public class CommandlineDiffblueTest {
  /**
   * Test Argument {@link Argument#getParts()}.
   * <ul>
   *   <li>Given {@link Argument} (default constructor) Line is empty string.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#getParts()}
   */
  @Test
  public void testArgumentGetParts_givenArgumentLineIsEmptyString_thenReturnArrayLengthIsZero() {
    // Arrange
    Argument argument = new Argument();
    argument.setLine("");

    // Act and Assert
    assertEquals(0, argument.getParts().length);
  }

  /**
   * Test Argument {@link Argument#getParts()}.
   * <ul>
   *   <li>Given {@link Argument} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#getParts()}
   */
  @Test
  public void testArgumentGetParts_givenArgument_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Argument()).getParts());
  }

  /**
   * Test Argument {@link Argument#getParts()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#getParts()}
   */
  @Test
  public void testArgumentGetParts_thenReturnArrayOfStringWithNull() {
    // Arrange
    Argument argument = new Argument();
    argument.setValue(null);
    argument.setPrefix("");
    argument.setSuffix("");

    // Act and Assert
    assertArrayEquals(new String[]{null}, argument.getParts());
  }

  /**
   * Test Argument {@link Argument#getParts()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code nullSuffix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#getParts()}
   */
  @Test
  public void testArgumentGetParts_thenReturnArrayOfStringWithNullSuffix() {
    // Arrange
    Argument argument = new Argument();
    argument.setValue(null);
    argument.setPrefix("");
    argument.setSuffix("Suffix");

    // Act and Assert
    assertArrayEquals(new String[]{"nullSuffix"}, argument.getParts());
  }

  /**
   * Test Argument {@link Argument#getParts()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code Prefixnull}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#getParts()}
   */
  @Test
  public void testArgumentGetParts_thenReturnArrayOfStringWithPrefixnull() {
    // Arrange
    Argument argument = new Argument();
    argument.setValue(null);
    argument.setPrefix("Prefix");
    argument.setSuffix("");

    // Act and Assert
    assertArrayEquals(new String[]{"Prefixnull"}, argument.getParts());
  }

  /**
   * Test Argument new {@link Argument} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Argument}
   */
  @Test
  public void testArgumentNewArgument() {
    // Arrange and Act
    Argument actualArgument = new Argument();

    // Assert
    assertNull(actualArgument.getParts());
    Location location = actualArgument.getLocation();
    assertNull(location.getFileName());
    assertNull(actualArgument.getDescription());
    assertNull(actualArgument.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test Argument {@link Argument#setFile(File)}.
   * <p>
   * Method under test: {@link Argument#setFile(File)}
   */
  @Test
  public void testArgumentSetFile() {
    // Arrange
    Argument argument = new Argument();

    // Act
    argument.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertArrayEquals(new String[]{Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()},
        argument.getParts());
  }

  /**
   * Test Argument {@link Argument#setLine(String)}.
   * <ul>
   *   <li>Then {@link Argument} (default constructor) Parts is array of {@link String} with {@code '}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#setLine(String)}
   */
  @Test
  public void testArgumentSetLine_thenArgumentPartsIsArrayOfStringWithApostrophe() {
    // Arrange
    Argument argument = new Argument();

    // Act
    argument.setLine("\"' \"");

    // Assert
    assertArrayEquals(new String[]{"' "}, argument.getParts());
  }

  /**
   * Test Argument {@link Argument#setLine(String)}.
   * <ul>
   *   <li>Then {@link Argument} (default constructor) Parts is array of {@link String} with {@code "}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#setLine(String)}
   */
  @Test
  public void testArgumentSetLine_thenArgumentPartsIsArrayOfStringWithQuotationMark() {
    // Arrange
    Argument argument = new Argument();

    // Act
    argument.setLine("'\"' ");

    // Assert
    assertArrayEquals(new String[]{"\""}, argument.getParts());
  }

  /**
   * Test Argument {@link Argument#setLine(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#setLine(String)}
   */
  @Test
  public void testArgumentSetLine_whenEmptyString_thenArrayLengthIsZero() {
    // Arrange
    Argument argument = new Argument();

    // Act
    argument.setLine("");

    // Assert
    assertEquals(0, argument.getParts().length);
  }

  /**
   * Test Argument {@link Argument#setLine(String)}.
   * <ul>
   *   <li>When {@code Line}.</li>
   *   <li>Then {@link Argument} (default constructor) Parts is array of {@link String} with {@code Line}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#setLine(String)}
   */
  @Test
  public void testArgumentSetLine_whenLine_thenArgumentPartsIsArrayOfStringWithLine() {
    // Arrange
    Argument argument = new Argument();

    // Act
    argument.setLine("Line");

    // Assert
    assertArrayEquals(new String[]{"Line"}, argument.getParts());
  }

  /**
   * Test Argument {@link Argument#setLine(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Argument} (default constructor) Parts is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#setLine(String)}
   */
  @Test
  public void testArgumentSetLine_whenNull_thenArgumentPartsIsNull() {
    // Arrange
    Argument argument = new Argument();

    // Act
    argument.setLine(null);

    // Assert that nothing has changed
    assertNull(argument.getParts());
  }

  /**
   * Test Argument {@link Argument#setLine(String)}.
   * <ul>
   *   <li>When space.</li>
   *   <li>Then array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#setLine(String)}
   */
  @Test
  public void testArgumentSetLine_whenSpace_thenArrayLengthIsZero() {
    // Arrange
    Argument argument = new Argument();

    // Act
    argument.setLine(" ");

    // Assert
    assertEquals(0, argument.getParts().length);
  }

  /**
   * Test Argument {@link Argument#setPath(Path)}.
   * <ul>
   *   <li>Then {@link Argument} (default constructor) Parts is array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#setPath(Path)}
   */
  @Test
  public void testArgumentSetPath_thenArgumentPartsIsArrayOfStringWithEmptyString() {
    // Arrange
    Argument argument = new Argument();

    // Act
    argument.setPath(new Path(new Project()));

    // Assert
    assertArrayEquals(new String[]{""}, argument.getParts());
  }

  /**
   * Test Argument {@link Argument#setPath(Path)}.
   * <ul>
   *   <li>Then {@link Argument} (default constructor) Parts is array of {@link String} with Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#setPath(Path)}
   */
  @Test
  public void testArgumentSetPath_thenArgumentPartsIsArrayOfStringWithPropertyIsUserDir() {
    // Arrange
    Argument argument = new Argument();

    // Act
    argument.setPath(new Path(new Project(), "."));

    // Assert
    assertArrayEquals(new String[]{System.getProperty("user.dir")}, argument.getParts());
  }

  /**
   * Test Argument {@link Argument#setPathref(Reference)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Argument} (default constructor) Parts is array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Argument#setPathref(Reference)}
   */
  @Test
  public void testArgumentSetPathref_whenNull_thenArgumentPartsIsArrayOfStringWithEmptyString() {
    // Arrange
    Argument argument = new Argument();

    // Act
    argument.setPathref(null);

    // Assert
    assertArrayEquals(new String[]{""}, argument.getParts());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Commandline#Commandline()}
   *   <li>{@link Commandline#toString()}
   *   <li>{@link Commandline#getExecutable()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Commandline actualCommandline = new Commandline();
    String actualToStringResult = actualCommandline.toString();

    // Assert
    assertEquals("", actualToStringResult);
    assertNull(actualCommandline.getExecutable());
  }

  /**
   * Test Marker {@link Marker#getPosition()}.
   * <ul>
   *   <li>Given {@link Commandline#Commandline(String)} with toProcess is {@code null}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Marker#getPosition()}
   */
  @Test
  public void testMarkerGetPosition_givenCommandlineWithToProcessIsNull_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, ((new Commandline(null)).new Marker(1)).getPosition());
  }

  /**
   * Test Marker {@link Marker#getPosition()}.
   * <ul>
   *   <li>Given {@link Commandline#Commandline(String)} with {@code To Process}.</li>
   *   <li>Then return two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Marker#getPosition()}
   */
  @Test
  public void testMarkerGetPosition_givenCommandlineWithToProcess_thenReturnTwo() {
    // Arrange, Act and Assert
    assertEquals(2, ((new Commandline("To Process")).new Marker(1)).getPosition());
  }

  /**
   * Test Marker getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Marker#Marker(Commandline, int)}
   *   <li>{@link Marker#getPrefix()}
   *   <li>{@link Marker#getSuffix()}
   * </ul>
   */
  @Test
  public void testMarkerGettersAndSetters() {
    // Arrange and Act
    Marker actualMarker = (new Commandline("To Process")).new Marker(1);
    String actualPrefix = actualMarker.getPrefix();

    // Assert
    assertEquals("", actualPrefix);
    assertEquals("", actualMarker.getSuffix());
  }

  /**
   * Test Marker {@link Marker#setPrefix(String)}.
   * <p>
   * Method under test: {@link Marker#setPrefix(String)}
   */
  @Test
  public void testMarkerSetPrefix() {
    // Arrange
    Marker marker = (new Commandline("To Process")).new Marker(1);

    // Act
    marker.setPrefix("Prefix");

    // Assert
    assertEquals("Prefix", marker.getPrefix());
  }

  /**
   * Test Marker {@link Marker#setPrefix(String)}.
   * <p>
   * Method under test: {@link Marker#setPrefix(String)}
   */
  @Test
  public void testMarkerSetPrefix2() {
    // Arrange
    Marker marker = (new Commandline("To Process")).new Marker(1);

    // Act
    marker.setPrefix(null);

    // Assert that nothing has changed
    assertEquals("", marker.getPrefix());
  }

  /**
   * Test Marker {@link Marker#setSuffix(String)}.
   * <p>
   * Method under test: {@link Marker#setSuffix(String)}
   */
  @Test
  public void testMarkerSetSuffix() {
    // Arrange
    Marker marker = (new Commandline("To Process")).new Marker(1);

    // Act
    marker.setSuffix("Suffix");

    // Assert
    assertEquals("Suffix", marker.getSuffix());
  }

  /**
   * Test Marker {@link Marker#setSuffix(String)}.
   * <p>
   * Method under test: {@link Marker#setSuffix(String)}
   */
  @Test
  public void testMarkerSetSuffix2() {
    // Arrange
    Marker marker = (new Commandline("To Process")).new Marker(1);

    // Act
    marker.setSuffix(null);

    // Assert that nothing has changed
    assertEquals("", marker.getSuffix());
  }

  /**
   * Test {@link Commandline#Commandline(String)}.
   * <ul>
   *   <li>Then return Executable is {@code '}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#Commandline(String)}
   */
  @Test
  public void testNewCommandline_thenReturnExecutableIsApostrophe() {
    // Arrange and Act
    Commandline actualCommandline = new Commandline("\"' \"");

    // Assert
    assertEquals("' ", actualCommandline.getExecutable());
    assertEquals(1, actualCommandline.size());
    assertArrayEquals(new String[]{"' "}, actualCommandline.getCommandline());
  }

  /**
   * Test {@link Commandline#Commandline(String)}.
   * <ul>
   *   <li>Then return Executable is {@code "}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#Commandline(String)}
   */
  @Test
  public void testNewCommandline_thenReturnExecutableIsQuotationMark() {
    // Arrange and Act
    Commandline actualCommandline = new Commandline("'\"' ");

    // Assert
    assertEquals("\"", actualCommandline.getExecutable());
    assertEquals(1, actualCommandline.size());
    assertArrayEquals(new String[]{"\""}, actualCommandline.getCommandline());
  }

  /**
   * Test {@link Commandline#Commandline(String)}.
   * <ul>
   *   <li>When {@code ''}.</li>
   *   <li>Then return Executable is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#Commandline(String)}
   */
  @Test
  public void testNewCommandline_whenApostropheApostrophe_thenReturnExecutableIsNull() {
    // Arrange and Act
    Commandline actualCommandline = new Commandline("''");

    // Assert
    assertNull(actualCommandline.getExecutable());
    assertEquals(0, actualCommandline.size());
    assertEquals(0, actualCommandline.getArguments().length);
    assertEquals(0, actualCommandline.getCommandline().length);
    assertFalse(actualCommandline.iterator().hasNext());
  }

  /**
   * Test {@link Commandline#Commandline(String)}.
   * <ul>
   *   <li>When {@code '}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#Commandline(String)}
   */
  @Test
  public void testNewCommandline_whenApostrophe_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> new Commandline("'"));
  }

  /**
   * Test {@link Commandline#Commandline(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Executable is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#Commandline(String)}
   */
  @Test
  public void testNewCommandline_whenEmptyString_thenReturnExecutableIsNull() {
    // Arrange and Act
    Commandline actualCommandline = new Commandline("");

    // Assert
    assertNull(actualCommandline.getExecutable());
    assertEquals(0, actualCommandline.size());
    assertEquals(0, actualCommandline.getArguments().length);
    assertEquals(0, actualCommandline.getCommandline().length);
    assertFalse(actualCommandline.iterator().hasNext());
  }

  /**
   * Test {@link Commandline#Commandline(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return Executable is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#Commandline(String)}
   */
  @Test
  public void testNewCommandline_whenNull_thenReturnExecutableIsNull() {
    // Arrange and Act
    Commandline actualCommandline = new Commandline(null);

    // Assert
    assertNull(actualCommandline.getExecutable());
    assertEquals(0, actualCommandline.size());
    assertEquals(0, actualCommandline.getArguments().length);
    assertEquals(0, actualCommandline.getCommandline().length);
    assertFalse(actualCommandline.iterator().hasNext());
  }

  /**
   * Test {@link Commandline#Commandline(String)}.
   * <ul>
   *   <li>When {@code "' "'}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#Commandline(String)}
   */
  @Test
  public void testNewCommandline_whenQuotationMarkApostropheSpaceQuotationMarkApostrophe() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> new Commandline("\"' \"' "));
  }

  /**
   * Test {@link Commandline#Commandline(String)}.
   * <ul>
   *   <li>When {@code "'}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#Commandline(String)}
   */
  @Test
  public void testNewCommandline_whenQuotationMarkApostrophe_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> new Commandline("\"' "));
  }

  /**
   * Test {@link Commandline#Commandline(String)}.
   * <ul>
   *   <li>When space.</li>
   *   <li>Then return Executable is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#Commandline(String)}
   */
  @Test
  public void testNewCommandline_whenSpace_thenReturnExecutableIsNull() {
    // Arrange and Act
    Commandline actualCommandline = new Commandline(" ");

    // Assert
    assertNull(actualCommandline.getExecutable());
    assertEquals(0, actualCommandline.size());
    assertEquals(0, actualCommandline.getArguments().length);
    assertEquals(0, actualCommandline.getCommandline().length);
    assertFalse(actualCommandline.iterator().hasNext());
  }

  /**
   * Test {@link Commandline#Commandline(String)}.
   * <ul>
   *   <li>When {@code To Process}.</li>
   *   <li>Then return Executable is {@code To}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#Commandline(String)}
   */
  @Test
  public void testNewCommandline_whenToProcess_thenReturnExecutableIsTo() {
    // Arrange and Act
    Commandline actualCommandline = new Commandline("To Process");

    // Assert
    assertEquals("To", actualCommandline.getExecutable());
    Iterator<Argument> iteratorResult = actualCommandline.iterator();
    Argument nextResult = iteratorResult.next();
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(1, nextResult.getParts().length);
    assertEquals(2, actualCommandline.size());
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"Process"}, actualCommandline.getArguments());
    assertArrayEquals(new String[]{"To", "Process"}, actualCommandline.getCommandline());
  }

  /**
   * Test {@link Commandline#createArgument(boolean)} with {@code boolean}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return Parts is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#createArgument(boolean)}
   */
  @Test
  public void testCreateArgumentWithBoolean_whenFalse_thenReturnPartsIsNull() {
    // Arrange and Act
    Argument actualCreateArgumentResult = (new Commandline("To Process")).createArgument(false);

    // Assert
    assertNull(actualCreateArgumentResult.getParts());
    Location location = actualCreateArgumentResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateArgumentResult.getDescription());
    assertNull(actualCreateArgumentResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link Commandline#setExecutable(String)} with {@code executable}.
   * <p>
   * Method under test: {@link Commandline#setExecutable(String)}
   */
  @Test
  public void testSetExecutableWithExecutable() {
    // Arrange
    Commandline commandline = new Commandline("To Process");

    // Act
    commandline.setExecutable("Executable");

    // Assert
    assertEquals("Executable", commandline.getExecutable());
    String[] commandline2 = commandline.getCommandline();
    assertEquals("Executable", commandline2[0]);
    assertEquals(2, commandline2.length);
  }

  /**
   * Test {@link Commandline#setExecutable(String, boolean)} with {@code executable}, {@code translateFileSeparator}.
   * <p>
   * Method under test: {@link Commandline#setExecutable(String, boolean)}
   */
  @Test
  public void testSetExecutableWithExecutableTranslateFileSeparator() {
    // Arrange
    Commandline commandline = new Commandline("To Process");

    // Act
    commandline.setExecutable("Executable", false);

    // Assert
    assertEquals("Executable", commandline.getExecutable());
    String[] commandline2 = commandline.getCommandline();
    assertEquals("Executable", commandline2[0]);
    assertEquals(2, commandline2.length);
  }

  /**
   * Test {@link Commandline#setExecutable(String, boolean)} with {@code executable}, {@code translateFileSeparator}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#setExecutable(String, boolean)}
   */
  @Test
  public void testSetExecutableWithExecutableTranslateFileSeparator_whenEmptyString() {
    // Arrange
    Commandline commandline = new Commandline("To Process");

    // Act
    commandline.setExecutable("", false);

    // Assert that nothing has changed
    assertEquals("To", commandline.getExecutable());
    String[] commandline2 = commandline.getCommandline();
    assertEquals("To", commandline2[0]);
    assertEquals(2, commandline2.length);
  }

  /**
   * Test {@link Commandline#setExecutable(String, boolean)} with {@code executable}, {@code translateFileSeparator}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#setExecutable(String, boolean)}
   */
  @Test
  public void testSetExecutableWithExecutableTranslateFileSeparator_whenNull() {
    // Arrange
    Commandline commandline = new Commandline("To Process");

    // Act
    commandline.setExecutable(null, false);

    // Assert that nothing has changed
    assertEquals("To", commandline.getExecutable());
    String[] commandline2 = commandline.getCommandline();
    assertEquals("To", commandline2[0]);
    assertEquals(2, commandline2.length);
  }

  /**
   * Test {@link Commandline#setExecutable(String, boolean)} with {@code executable}, {@code translateFileSeparator}.
   * <ul>
   *   <li>When {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#setExecutable(String, boolean)}
   */
  @Test
  public void testSetExecutableWithExecutableTranslateFileSeparator_whenTrue() {
    // Arrange
    Commandline commandline = new Commandline("To Process");

    // Act
    commandline.setExecutable("Executable", true);

    // Assert
    assertEquals("Executable", commandline.getExecutable());
    String[] commandline2 = commandline.getCommandline();
    assertEquals("Executable", commandline2[0]);
    assertEquals(2, commandline2.length);
  }

  /**
   * Test {@link Commandline#setExecutable(String)} with {@code executable}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#setExecutable(String)}
   */
  @Test
  public void testSetExecutableWithExecutable_whenEmptyString() {
    // Arrange
    Commandline commandline = new Commandline("To Process");

    // Act
    commandline.setExecutable("");

    // Assert that nothing has changed
    assertEquals("To", commandline.getExecutable());
    String[] commandline2 = commandline.getCommandline();
    assertEquals("To", commandline2[0]);
    assertEquals(2, commandline2.length);
  }

  /**
   * Test {@link Commandline#setExecutable(String)} with {@code executable}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#setExecutable(String)}
   */
  @Test
  public void testSetExecutableWithExecutable_whenNull() {
    // Arrange
    Commandline commandline = new Commandline("To Process");

    // Act
    commandline.setExecutable(null);

    // Assert that nothing has changed
    assertEquals("To", commandline.getExecutable());
    String[] commandline2 = commandline.getCommandline();
    assertEquals("To", commandline2[0]);
    assertEquals(2, commandline2.length);
  }

  /**
   * Test {@link Commandline#getCommandline()}.
   * <ul>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#getCommandline()}
   */
  @Test
  public void testGetCommandline_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Commandline(null)).getCommandline().length);
  }

  /**
   * Test {@link Commandline#getCommandline()}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code To} and {@code Process}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#getCommandline()}
   */
  @Test
  public void testGetCommandline_thenReturnArrayOfStringWithToAndProcess() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"To", "Process"}, (new Commandline("To Process")).getCommandline());
  }

  /**
   * Test {@link Commandline#getArguments()}.
   * <p>
   * Method under test: {@link Commandline#getArguments()}
   */
  @Test
  public void testGetArguments() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"Process"}, (new Commandline("To Process")).getArguments());
  }

  /**
   * Test {@link Commandline#toString(String[])} with {@code String[]}.
   * <p>
   * Method under test: {@link Commandline#toString(String[])}
   */
  @Test
  public void testToStringWithString() {
    // Arrange, Act and Assert
    assertEquals("'\"' \"'\"", Commandline.toString(new String[]{"\"", "'"}));
  }

  /**
   * Test {@link Commandline#toString(String[])} with {@code String[]}.
   * <ul>
   *   <li>Then return {@code '"'}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#toString(String[])}
   */
  @Test
  public void testToStringWithString_thenReturnApostropheQuotationMarkApostrophe() {
    // Arrange, Act and Assert
    assertEquals("'\"'", Commandline.toString(new String[]{"\""}));
  }

  /**
   * Test {@link Commandline#toString(String[])} with {@code String[]}.
   * <ul>
   *   <li>Then return {@code "'"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#toString(String[])}
   */
  @Test
  public void testToStringWithString_thenReturnQuotationMarkApostropheQuotationMark() {
    // Arrange, Act and Assert
    assertEquals("\"'\"", Commandline.toString(new String[]{"'"}));
  }

  /**
   * Test {@link Commandline#toString(String[])} with {@code String[]}.
   * <ul>
   *   <li>Then return {@code " "}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#toString(String[])}
   */
  @Test
  public void testToStringWithString_thenReturnQuotationMarkSpaceQuotationMark() {
    // Arrange, Act and Assert
    assertEquals("\" \"", Commandline.toString(new String[]{" "}));
  }

  /**
   * Test {@link Commandline#toString(String[])} with {@code String[]}.
   * <ul>
   *   <li>When array of {@link String} with {@code Line}.</li>
   *   <li>Then return {@code Line}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#toString(String[])}
   */
  @Test
  public void testToStringWithString_whenArrayOfStringWithLine_thenReturnLine() {
    // Arrange, Act and Assert
    assertEquals("Line", Commandline.toString(new String[]{"Line"}));
  }

  /**
   * Test {@link Commandline#toString(String[])} with {@code String[]}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#toString(String[])}
   */
  @Test
  public void testToStringWithString_whenEmptyArrayOfString_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Commandline.toString(new String[]{}));
  }

  /**
   * Test {@link Commandline#toString(String[])} with {@code String[]}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#toString(String[])}
   */
  @Test
  public void testToStringWithString_whenNull_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Commandline.toString(null));
  }

  /**
   * Test {@link Commandline#translateCommandline(String)}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code '}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#translateCommandline(String)}
   */
  @Test
  public void testTranslateCommandline_thenReturnArrayOfStringWithApostrophe() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"' "}, Commandline.translateCommandline("\"' \""));
  }

  /**
   * Test {@link Commandline#translateCommandline(String)}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code "}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#translateCommandline(String)}
   */
  @Test
  public void testTranslateCommandline_thenReturnArrayOfStringWithQuotationMark() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"\""}, Commandline.translateCommandline("'\"' "));
  }

  /**
   * Test {@link Commandline#translateCommandline(String)}.
   * <ul>
   *   <li>When {@code '}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#translateCommandline(String)}
   */
  @Test
  public void testTranslateCommandline_whenApostrophe_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> Commandline.translateCommandline("'"));
  }

  /**
   * Test {@link Commandline#translateCommandline(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#translateCommandline(String)}
   */
  @Test
  public void testTranslateCommandline_whenEmptyString_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Commandline.translateCommandline("").length);
  }

  /**
   * Test {@link Commandline#translateCommandline(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#translateCommandline(String)}
   */
  @Test
  public void testTranslateCommandline_whenNull_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Commandline.translateCommandline(null).length);
  }

  /**
   * Test {@link Commandline#translateCommandline(String)}.
   * <ul>
   *   <li>When {@code "' "'}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#translateCommandline(String)}
   */
  @Test
  public void testTranslateCommandline_whenQuotationMarkApostropheSpaceQuotationMarkApostrophe() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> Commandline.translateCommandline("\"' \"' "));
  }

  /**
   * Test {@link Commandline#translateCommandline(String)}.
   * <ul>
   *   <li>When {@code "'}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#translateCommandline(String)}
   */
  @Test
  public void testTranslateCommandline_whenQuotationMarkApostrophe_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> Commandline.translateCommandline("\"' "));
  }

  /**
   * Test {@link Commandline#translateCommandline(String)}.
   * <ul>
   *   <li>When space.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#translateCommandline(String)}
   */
  @Test
  public void testTranslateCommandline_whenSpace_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, Commandline.translateCommandline(" ").length);
  }

  /**
   * Test {@link Commandline#translateCommandline(String)}.
   * <ul>
   *   <li>When {@code To Process}.</li>
   *   <li>Then return array of {@link String} with {@code To} and {@code Process}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#translateCommandline(String)}
   */
  @Test
  public void testTranslateCommandline_whenToProcess_thenReturnArrayOfStringWithToAndProcess() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"To", "Process"}, Commandline.translateCommandline("To Process"));
  }

  /**
   * Test {@link Commandline#quoteArgument(String)}.
   * <ul>
   *   <li>When {@code '}.</li>
   *   <li>Then return {@code "'"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#quoteArgument(String)}
   */
  @Test
  public void testQuoteArgument_whenApostrophe_thenReturnQuotationMarkApostropheQuotationMark() {
    // Arrange, Act and Assert
    assertEquals("\"'\"", Commandline.quoteArgument("'"));
  }

  /**
   * Test {@link Commandline#quoteArgument(String)}.
   * <ul>
   *   <li>When {@code Argument}.</li>
   *   <li>Then return {@code Argument}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#quoteArgument(String)}
   */
  @Test
  public void testQuoteArgument_whenArgument_thenReturnArgument() {
    // Arrange, Act and Assert
    assertEquals("Argument", Commandline.quoteArgument("Argument"));
  }

  /**
   * Test {@link Commandline#quoteArgument(String)}.
   * <ul>
   *   <li>When {@code "'}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#quoteArgument(String)}
   */
  @Test
  public void testQuoteArgument_whenQuotationMarkApostrophe_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> Commandline.quoteArgument("\"'"));
  }

  /**
   * Test {@link Commandline#quoteArgument(String)}.
   * <ul>
   *   <li>When {@code "}.</li>
   *   <li>Then return {@code '"'}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#quoteArgument(String)}
   */
  @Test
  public void testQuoteArgument_whenQuotationMark_thenReturnApostropheQuotationMarkApostrophe() {
    // Arrange, Act and Assert
    assertEquals("'\"'", Commandline.quoteArgument("\""));
  }

  /**
   * Test {@link Commandline#quoteArgument(String)}.
   * <ul>
   *   <li>When space.</li>
   *   <li>Then return {@code " "}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#quoteArgument(String)}
   */
  @Test
  public void testQuoteArgument_whenSpace_thenReturnQuotationMarkSpaceQuotationMark() {
    // Arrange, Act and Assert
    assertEquals("\" \"", Commandline.quoteArgument(" "));
  }

  /**
   * Test {@link Commandline#size()}.
   * <ul>
   *   <li>Given {@link Commandline#Commandline(String)} with toProcess is {@code null}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#size()}
   */
  @Test
  public void testSize_givenCommandlineWithToProcessIsNull_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Commandline(null)).size());
  }

  /**
   * Test {@link Commandline#size()}.
   * <ul>
   *   <li>Given {@link Commandline#Commandline(String)} with {@code To Process}.</li>
   *   <li>Then return two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#size()}
   */
  @Test
  public void testSize_givenCommandlineWithToProcess_thenReturnTwo() {
    // Arrange, Act and Assert
    assertEquals(2, (new Commandline("To Process")).size());
  }

  /**
   * Test {@link Commandline#clone()}.
   * <p>
   * Method under test: {@link Commandline#clone()}
   */
  @Test
  public void testClone() {
    // Arrange and Act
    Object actualCloneResult = (new Commandline("To Process")).clone();

    // Assert
    assertTrue(actualCloneResult instanceof Commandline);
    assertEquals("To", ((Commandline) actualCloneResult).getExecutable());
    Iterator<Argument> iteratorResult = ((Commandline) actualCloneResult).iterator();
    Argument nextResult = iteratorResult.next();
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(1, nextResult.getParts().length);
    assertEquals(2, ((Commandline) actualCloneResult).size());
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"Process"}, ((Commandline) actualCloneResult).getArguments());
    assertArrayEquals(new String[]{"To", "Process"}, ((Commandline) actualCloneResult).getCommandline());
  }

  /**
   * Test {@link Commandline#clear()}.
   * <p>
   * Method under test: {@link Commandline#clear()}
   */
  @Test
  public void testClear() {
    // Arrange
    Commandline commandline = new Commandline("To Process");

    // Act
    commandline.clear();

    // Assert
    assertNull(commandline.getExecutable());
    assertEquals(0, commandline.size());
    assertEquals(0, commandline.getArguments().length);
    assertEquals(0, commandline.getCommandline().length);
    assertFalse(commandline.iterator().hasNext());
  }

  /**
   * Test {@link Commandline#clearArgs()}.
   * <p>
   * Method under test: {@link Commandline#clearArgs()}
   */
  @Test
  public void testClearArgs() {
    // Arrange
    Commandline commandline = new Commandline("To Process");

    // Act
    commandline.clearArgs();

    // Assert
    assertEquals(0, commandline.getArguments().length);
    assertEquals(1, commandline.size());
    assertEquals(1, commandline.getCommandline().length);
    assertFalse(commandline.iterator().hasNext());
  }

  /**
   * Test {@link Commandline#createMarker()}.
   * <p>
   * Method under test: {@link Commandline#createMarker()}
   */
  @Test
  public void testCreateMarker() {
    // Arrange and Act
    Marker actualCreateMarkerResult = (new Commandline("To Process")).createMarker();

    // Assert
    assertEquals("", actualCreateMarkerResult.getPrefix());
    assertEquals("", actualCreateMarkerResult.getSuffix());
    assertEquals(2, actualCreateMarkerResult.getPosition());
  }

  /**
   * Test {@link Commandline#describeCommand()}.
   * <p>
   * Method under test: {@link Commandline#describeCommand()}
   */
  @Test
  public void testDescribeCommand() {
    // Arrange, Act and Assert
    assertEquals(
        "Executing 'with'\nThe ' characters around the executable and arguments are\nnot part of the command.\n",
        (new Commandline(" with ")).describeCommand());
  }

  /**
   * Test {@link Commandline#describeCommand(String[])} with {@code args}.
   * <p>
   * Method under test: {@link Commandline#describeCommand(String[])}
   */
  @Test
  public void testDescribeCommandWithArgs() {
    // Arrange, Act and Assert
    assertEquals(
        "Executing 'Args'\nThe ' characters around the executable and arguments are\nnot part of the command.\n",
        Commandline.describeCommand(new String[]{"Args"}));
  }

  /**
   * Test {@link Commandline#describeCommand(String[])} with {@code args}.
   * <ul>
   *   <li>Then return a string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeCommand(String[])}
   */
  @Test
  public void testDescribeCommandWithArgs_thenReturnAString() {
    // Arrange, Act and Assert
    assertEquals(
        "Executing 'Executing '' with arguments:\n" + "'''\n" + "\n"
            + "The ' characters around the executable and arguments are\n" + "not part of the command.\n",
        Commandline.describeCommand(new String[]{"Executing '", "'"}));
  }

  /**
   * Test {@link Commandline#describeCommand(String[])} with {@code args}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeCommand(String[])}
   */
  @Test
  public void testDescribeCommandWithArgs_whenEmptyArrayOfString_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Commandline.describeCommand(new String[]{}));
  }

  /**
   * Test {@link Commandline#describeCommand(String[])} with {@code args}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeCommand(String[])}
   */
  @Test
  public void testDescribeCommandWithArgs_whenNull_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Commandline.describeCommand((String[]) null));
  }

  /**
   * Test {@link Commandline#describeCommand(Commandline)} with {@code line}.
   * <p>
   * Method under test: {@link Commandline#describeCommand(Commandline)}
   */
  @Test
  public void testDescribeCommandWithLine() {
    // Arrange, Act and Assert
    assertEquals(
        "Executing 'with'\nThe ' characters around the executable and arguments are\nnot part of the command.\n",
        Commandline.describeCommand(new Commandline(" with ")));
  }

  /**
   * Test {@link Commandline#describeCommand(Commandline)} with {@code line}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeCommand(Commandline)}
   */
  @Test
  public void testDescribeCommandWithLine_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Commandline.describeCommand(new Commandline(null)));
  }

  /**
   * Test {@link Commandline#describeCommand(Commandline)} with {@code line}.
   * <ul>
   *   <li>When {@link Commandline#Commandline(String)} with {@code To Process}.</li>
   *   <li>Then return a string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeCommand(Commandline)}
   */
  @Test
  public void testDescribeCommandWithLine_whenCommandlineWithToProcess_thenReturnAString() {
    // Arrange, Act and Assert
    assertEquals(
        "Executing 'To' with arguments:\n" + "'Process'\n" + "\n"
            + "The ' characters around the executable and arguments are\n" + "not part of the command.\n",
        Commandline.describeCommand(new Commandline("To Process")));
  }

  /**
   * Test {@link Commandline#describeCommand()}.
   * <ul>
   *   <li>Given {@link Commandline#Commandline(String)} with toProcess is {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeCommand()}
   */
  @Test
  public void testDescribeCommand_givenCommandlineWithToProcessIsNull_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new Commandline(null)).describeCommand());
  }

  /**
   * Test {@link Commandline#describeCommand()}.
   * <ul>
   *   <li>Given {@link Commandline#Commandline(String)} with {@code To Process}.</li>
   *   <li>Then return a string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeCommand()}
   */
  @Test
  public void testDescribeCommand_givenCommandlineWithToProcess_thenReturnAString() {
    // Arrange, Act and Assert
    assertEquals(
        "Executing 'To' with arguments:\n" + "'Process'\n" + "\n"
            + "The ' characters around the executable and arguments are\n" + "not part of the command.\n",
        (new Commandline("To Process")).describeCommand());
  }

  /**
   * Test {@link Commandline#describeArguments(String[])} with {@code args}.
   * <p>
   * Method under test: {@link Commandline#describeArguments(String[])}
   */
  @Test
  public void testDescribeArgumentsWithArgs() {
    // Arrange, Act and Assert
    assertEquals("arguments:\n" + "'Args'\n" + "\n" + "The ' characters around the executable and arguments are\n"
        + "not part of the command.\n", Commandline.describeArguments(new String[]{"Args"}));
  }

  /**
   * Test {@link Commandline#describeArguments(String[], int)} with {@code args}, {@code offset}.
   * <p>
   * Method under test: {@link Commandline#describeArguments(String[], int)}
   */
  @Test
  public void testDescribeArgumentsWithArgsOffset() {
    // Arrange, Act and Assert
    assertEquals("arguments:\n" + "'Args'\n" + "\n" + "The ' characters around the executable and arguments are\n"
        + "not part of the command.\n", Commandline.describeArguments(new String[]{"Args", "", "Args"}, 2));
  }

  /**
   * Test {@link Commandline#describeArguments(String[], int)} with {@code args}, {@code offset}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeArguments(String[], int)}
   */
  @Test
  public void testDescribeArgumentsWithArgsOffset_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Commandline.describeArguments(new String[]{"Args"}, 2));
  }

  /**
   * Test {@link Commandline#describeArguments(String[], int)} with {@code args}, {@code offset}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeArguments(String[], int)}
   */
  @Test
  public void testDescribeArgumentsWithArgsOffset_whenNull_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Commandline.describeArguments(null, 2));
  }

  /**
   * Test {@link Commandline#describeArguments(String[])} with {@code args}.
   * <ul>
   *   <li>When empty array of {@link String}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeArguments(String[])}
   */
  @Test
  public void testDescribeArgumentsWithArgs_whenEmptyArrayOfString_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Commandline.describeArguments(new String[]{}));
  }

  /**
   * Test {@link Commandline#describeArguments(String[])} with {@code args}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeArguments(String[])}
   */
  @Test
  public void testDescribeArgumentsWithArgs_whenNull_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Commandline.describeArguments((String[]) null));
  }

  /**
   * Test {@link Commandline#describeArguments(Commandline)} with {@code line}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeArguments(Commandline)}
   */
  @Test
  public void testDescribeArgumentsWithLine_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", Commandline.describeArguments(new Commandline("argument%s:%n")));
  }

  /**
   * Test {@link Commandline#describeArguments(Commandline)} with {@code line}.
   * <ul>
   *   <li>When {@link Commandline#Commandline(String)} with {@code To Process}.</li>
   *   <li>Then return a string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeArguments(Commandline)}
   */
  @Test
  public void testDescribeArgumentsWithLine_whenCommandlineWithToProcess_thenReturnAString() {
    // Arrange, Act and Assert
    assertEquals("arguments:\n" + "'Process'\n" + "\n" + "The ' characters around the executable and arguments are\n"
        + "not part of the command.\n", Commandline.describeArguments(new Commandline("To Process")));
  }

  /**
   * Test {@link Commandline#describeArguments()}.
   * <ul>
   *   <li>Given {@link Commandline#Commandline(String)} with {@code To Process}.</li>
   *   <li>Then return a string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeArguments()}
   */
  @Test
  public void testDescribeArguments_givenCommandlineWithToProcess_thenReturnAString() {
    // Arrange, Act and Assert
    assertEquals("arguments:\n" + "'Process'\n" + "\n" + "The ' characters around the executable and arguments are\n"
        + "not part of the command.\n", (new Commandline("To Process")).describeArguments());
  }

  /**
   * Test {@link Commandline#describeArguments()}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Commandline#describeArguments()}
   */
  @Test
  public void testDescribeArguments_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new Commandline("argument%s:%n")).describeArguments());
  }

  /**
   * Test {@link Commandline#iterator()}.
   * <p>
   * Method under test: {@link Commandline#iterator()}
   */
  @Test
  public void testIterator() {
    // Arrange and Act
    Iterator<Argument> actualIteratorResult = (new Commandline("To Process")).iterator();

    // Assert
    Argument nextResult = actualIteratorResult.next();
    Location location = nextResult.getLocation();
    assertNull(location.getFileName());
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualIteratorResult.hasNext());
    assertArrayEquals(new String[]{"Process"}, nextResult.getParts());
  }
}
