package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import org.junit.Test;

public class MergingMapperDiffblueTest {
  /**
  * Methods under test: 
  * 
  * <ul>
  *   <li>{@link MergingMapper#MergingMapper()}
  *   <li>{@link MergingMapper#setTo(String)}
  *   <li>{@link MergingMapper#setFrom(String)}
  * </ul>
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    MergingMapper actualMergingMapper = new MergingMapper();
    actualMergingMapper.setTo("alice.liddell@example.org");
    actualMergingMapper.setFrom("jane.doe@example.org");

    // Assert that nothing has changed
    String[] stringArray = actualMergingMapper.mergedFile;
    assertEquals(1, stringArray.length);
    assertArrayEquals(new String[]{"alice.liddell@example.org"}, stringArray);
    assertEquals("alice.liddell@example.org", stringArray[0]);
  }

  /**
   * Method under test: {@link MergingMapper#MergingMapper(String)}
   */
  @Test
  public void testConstructor2() {
    // Arrange, Act and Assert
    assertEquals(1, (new MergingMapper("alice.liddell@example.org")).mergedFile.length);
  }

  /**
   * Method under test: {@link MergingMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName() {
    // Arrange
    MergingMapper mergingMapper = new MergingMapper("alice.liddell@example.org");

    // Act
    String[] actualMapFileNameResult = mergingMapper.mapFileName("foo.txt");

    // Assert
    assertSame(mergingMapper.mergedFile, actualMapFileNameResult);
    assertEquals(1, actualMapFileNameResult.length);
    assertEquals("alice.liddell@example.org", actualMapFileNameResult[0]);
  }
}

