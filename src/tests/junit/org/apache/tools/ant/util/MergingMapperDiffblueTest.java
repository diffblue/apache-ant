package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertSame;
import org.junit.Test;

public class MergingMapperDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link MergingMapper#MergingMapper()}
   *   <li>{@link MergingMapper#setTo(String)}
   *   <li>{@link MergingMapper#setFrom(String)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    MergingMapper actualMergingMapper = new MergingMapper();
    actualMergingMapper.setTo("alice.liddell@example.org");
    actualMergingMapper.setFrom("jane.doe@example.org");

    // Assert
    assertArrayEquals(new String[]{"alice.liddell@example.org"}, actualMergingMapper.mergedFile);
  }

  /**
   * Test {@link MergingMapper#MergingMapper(String)}.
   * <p>
   * Method under test: {@link MergingMapper#MergingMapper(String)}
   */
  @Test
  public void testNewMergingMapper() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"alice.liddell@example.org"},
        (new MergingMapper("alice.liddell@example.org")).mergedFile);
  }

  /**
   * Test {@link MergingMapper#mapFileName(String)}.
   * <p>
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
    assertArrayEquals(new String[]{"alice.liddell@example.org"}, actualMapFileNameResult);
  }
}
