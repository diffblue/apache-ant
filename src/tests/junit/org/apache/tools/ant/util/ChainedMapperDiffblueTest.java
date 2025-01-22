package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class ChainedMapperDiffblueTest {
  /**
   * Test {@link ChainedMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link ChainedMapper} (default constructor) addConfigured {@link CompositeMapper} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainedMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenChainedMapperAddConfiguredCompositeMapper_thenReturnNull() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    chainedMapper.addConfigured(new CompositeMapper());

    // Act and Assert
    assertNull(chainedMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link ChainedMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link ChainedMapper} (default constructor).</li>
   *   <li>Then return array of {@link String} with {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainedMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenChainedMapper_thenReturnArrayOfStringWithFooTxt() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, (new ChainedMapper()).mapFileName("foo.txt"));
  }

  /**
   * Test {@link ChainedMapper#mapFileName(String)}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChainedMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_thenReturnArrayOfStringWithFooTxt() {
    // Arrange
    ChainedMapper chainedMapper = new ChainedMapper();
    chainedMapper.addConfigured(new ChainedMapper());

    // Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, chainedMapper.mapFileName("foo.txt"));
  }

  /**
   * Test new {@link ChainedMapper} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ChainedMapper}
   */
  @Test
  public void testNewChainedMapper() {
    // Arrange, Act and Assert
    assertTrue((new ChainedMapper()).getMappers().isEmpty());
  }
}
