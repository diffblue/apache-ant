package org.apache.tools.ant.taskdefs.cvslib;

import static org.junit.Assert.assertEquals;
import java.util.ArrayList;
import java.util.List;
import org.apache.tools.ant.taskdefs.AbstractCvsTask;
import org.apache.tools.ant.taskdefs.AbstractCvsTask.Module;
import org.junit.Test;

public class ChangeLogParserDiffblueTest {
  /**
   * Test {@link ChangeLogParser#ChangeLogParser()}.
   * <p>
   * Method under test: {@link ChangeLogParser#ChangeLogParser()}
   */
  @Test
  public void testNewChangeLogParser() {
    // Arrange, Act and Assert
    assertEquals(0, (new ChangeLogParser()).getEntrySetAsArray().length);
  }

  /**
   * Test {@link ChangeLogParser#ChangeLogParser(boolean, String, List)}.
   * <ul>
   *   <li>Given {@link Module} (default constructor) Name is {@code yyyy-MM-dd HH:mm:ss Z}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@link Module} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ChangeLogParser#ChangeLogParser(boolean, String, List)}
   */
  @Test
  public void testNewChangeLogParser_givenModuleNameIsYyyyMmDdHhMmSsZ_whenArrayListAddModule() {
    // Arrange
    Module resultModule = new Module();
    resultModule.setName("yyyy/MM/dd HH:mm:ss");

    Module resultModule2 = new Module();
    resultModule2.setName("yyyy-MM-dd HH:mm:ss Z");

    ArrayList<Module> modules = new ArrayList<>();
    modules.add(resultModule2);
    modules.add(resultModule);

    // Act and Assert
    assertEquals(0, (new ChangeLogParser(true, "java.text", modules)).getEntrySetAsArray().length);
  }

  /**
   * Test {@link ChangeLogParser#ChangeLogParser(boolean, String, List)}.
   * <ul>
   *   <li>Given {@link Module} (default constructor) Name is {@code yyyy/MM/dd HH:mm:ss}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@link Module} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ChangeLogParser#ChangeLogParser(boolean, String, List)}
   */
  @Test
  public void testNewChangeLogParser_givenModuleNameIsYyyyMmDdHhMmSs_whenArrayListAddModule() {
    // Arrange
    Module resultModule = new Module();
    resultModule.setName("yyyy/MM/dd HH:mm:ss");

    ArrayList<Module> modules = new ArrayList<>();
    modules.add(resultModule);

    // Act and Assert
    assertEquals(0, (new ChangeLogParser(true, "java.text", modules)).getEntrySetAsArray().length);
  }

  /**
   * Test {@link ChangeLogParser#ChangeLogParser(boolean, String, List)}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChangeLogParser#ChangeLogParser(boolean, String, List)}
   */
  @Test
  public void testNewChangeLogParser_whenArrayList_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new ChangeLogParser(true, "java.text", new ArrayList<>())).getEntrySetAsArray().length);
  }

  /**
   * Test {@link ChangeLogParser#ChangeLogParser(boolean, String, List)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChangeLogParser#ChangeLogParser(boolean, String, List)}
   */
  @Test
  public void testNewChangeLogParser_whenNull_thenReturnArrayLengthIsZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new ChangeLogParser(true, null, new ArrayList<>())).getEntrySetAsArray().length);
  }

  /**
   * Test {@link ChangeLogParser#getEntrySetAsArray()}.
   * <p>
   * Method under test: {@link ChangeLogParser#getEntrySetAsArray()}
   */
  @Test
  public void testGetEntrySetAsArray() {
    // Arrange, Act and Assert
    assertEquals(0, (new ChangeLogParser()).getEntrySetAsArray().length);
  }
}
